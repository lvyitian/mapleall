/*
 * Copyright (c) [2020] Huawei Technologies Co.,Ltd.All rights reserved.
 *
 * OpenArkCompiler is licensed under the Mulan PSL v1.
 * You can use this software according to the terms and conditions of the Mulan PSL v1.
 * You may obtain a copy of Mulan PSL v1 at:
 *
 *     http://license.coscl.org.cn/MulanPSL
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND, EITHER
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT, MERCHANTABILITY OR
 * FIT FOR A PARTICULAR PURPOSE.
 * See the Mulan PSL v1 for more details.
 */

#include "name_mangler.h"
#include <regex>
#include <assert.h>
#include <map>

namespace NameMangler {

const int kLocalCodebufSize = 1024;
#define MAX_CODECBUF_SIZE (1<<16) // Java spec support a max name length of 64K.

#define GETHEXCHAR(n) (char)((n) < 10 ? (n)+'0' : (n)-10+'a')
#define GETHEXCHARU(n) (char)((n) < 10 ? (n)+'0' : (n)-10+'A')

bool doCompression = false;

// Store a mapping between full string and its compressed version
// More frequent and specific strings go before  general ones,
// e.g. Ljava_2Flang_2FObject_3B goes before Ljava_2Flang_2F
//
using StringMap = std::map<const std::string, const std::string>;

static const StringMap kInternalMangleTable = {
  {"Ljava_2Flang_2FObject_3B", "L0_3B"},
  {"Ljava_2Flang_2FClass_3B",  "L1_3B"},
  {"Ljava_2Flang_2FString_3B", "L2_3B"}
};

// This mapping is mainly used for compressing annotation strings
static const StringMap kOriginalMangleTable = {
  {"Ljava/lang/Object", "L0"},
  {"Ljava/lang/Class",  "L1"},
  {"Ljava/lang/String", "L2"}
};

// The returned buffer needs to be explicitly freed
static inline char* AllocCodecBuf(size_t maxlen) {
  if (maxlen == 0)
    return nullptr;
  return (char *)malloc((maxlen <= kLocalCodebufSize) ? 3*maxlen : 3 * MAX_CODECBUF_SIZE);
}

static inline void FreeCodecBuf(char* buf) {
  free(buf);
}

static std::string CompressName(std::string& name, const StringMap& mapping = kInternalMangleTable) {
  for (auto& entry : mapping) {
    if (name.find(entry.first) != name.npos) {
      name = std::regex_replace(name, std::regex(entry.first), entry.second);
    }
  }
  return name;
}

static std::string CompressName(const char* name, const StringMap& mapping = kInternalMangleTable) {
  std::string nameStr(name);
  return CompressName(nameStr, mapping);
}

static std::string DecompressName(std::string& name, const StringMap& mapping = kInternalMangleTable) {
  for (auto& entry : mapping) {
    if (name.find(entry.second) != name.npos) {
      name = std::regex_replace(name, std::regex(entry.second), entry.first);
    }
  }
  return name;
}

std::string GetInternalNameLiteral(const char* name) {
  return (doCompression ? CompressName(name) : std::string(name));
}

std::string GetOriginalNameLiteral(const char* name) {
  return (doCompression ? CompressName(name, kOriginalMangleTable) : std::string(name));
}

std::string EncodeName(const std::string& name) {
  return EncodeName(name.c_str());
}

std::string EncodeName(const char *name) {
  // name is guaranteed to be null-terminated
  size_t namelen = strlen(name);
  namelen = namelen > MAX_CODECBUF_SIZE ? MAX_CODECBUF_SIZE : namelen;
  char *buf = AllocCodecBuf(namelen);
  if (!buf)
    return std::string(name);

  size_t pos = 0;
  size_t i = 0;
  std::string str(name);
  std::u16string str16;
  while (i < namelen) {
    unsigned char c = name[i];
    if (c == '_') {
      buf[pos++] = '_';
      buf[pos++] = '_';
    } else if (c == '[') {
      buf[pos++] = 'A';
    } else if (isalnum(c)) { // TODO: a inlined version of isalnum()
      buf[pos++] = c;
    } else if (c <= 0x7F) {
      // _XX: '_' followed by ascii code in hex
      if (c == '.')
        c = '/'; // use / in package name
      buf[pos++] = '_';
      unsigned char n = c>>4; // c/16
      buf[pos++] = GETHEXCHARU(n);
      n = c - (n<<4);
      buf[pos++] = GETHEXCHARU(n);
    } else {
      str16.clear();
      // process one 16-bit char at a time
      unsigned int n = UTF8ToUTF16(str16, str.substr(i), 1, false);
      buf[pos++] = '_';
      if ((n >> 16) == 1) {
        unsigned short m = str16[0];
        buf[pos++] = 'u';
        buf[pos++] = GETHEXCHAR((m&0xF000)>>12);
        buf[pos++] = GETHEXCHAR((m&0x0F00)>> 8);
        buf[pos++] = GETHEXCHAR((m&0x00F0)>> 4);
        buf[pos++] = GETHEXCHAR(m&0x000F);
      } else {
        unsigned short m = str16[0];
        buf[pos++] = 'U';
        buf[pos++] = GETHEXCHAR((m&0xF000)>>12);
        buf[pos++] = GETHEXCHAR((m&0x0F00)>> 8);
        buf[pos++] = GETHEXCHAR((m&0x00F0)>> 4);
        buf[pos++] = GETHEXCHAR(m&0x000F);
        m = str16[1];
        buf[pos++] = GETHEXCHAR((m&0xF000)>>12);
        buf[pos++] = GETHEXCHAR((m&0x0F00)>> 8);
        buf[pos++] = GETHEXCHAR((m&0x00F0)>> 4);
        buf[pos++] = GETHEXCHAR(m&0x000F);
      }
      i += int32_t(n & 0xFFFF) - 1;
    }

    i++;
  }

  buf[pos] = '\0';
  std::string newName = std::string(buf, pos);
  FreeCodecBuf(buf);
  if (doCompression) {
    newName = CompressName(newName);
  }
  return newName;
}

static inline bool UpdatePrimType(bool primType, int splitNo, unsigned int c) {
  if (c == 'L') {
    return false;
  }

  if (((c == ';') || (c == '(') || (c == ')'))
      && (splitNo > 1)) {
    return true;
  }

  return primType;
}

std::string DecodeName(const std::string& name) {
  if (name.find(';') != std::string::npos) { // no need Decoding a non-encoded string
    return name;
  }
  // MRT functions starting with MRT_, _MRT_ or CC_
  if (name.find("MRT_") == 0 || name.find("_MRT_") == 0 || name.find("CC_") == 0) return name;
  std::string decompressedName;
  const char* namePtr;
  size_t nameLen;

  if (doCompression) {
    decompressedName = name;
    decompressedName = DecompressName(decompressedName);
    namePtr = decompressedName.c_str();
    nameLen = decompressedName.length();
  }
  else {
    namePtr = name.c_str();
    nameLen = name.length();
  }

  // Demangled name is supposed to be shorter. No buffer overflow issue here.
  std::string newName(nameLen, '\0');

  bool primType = true;
  int splitNo = 0; // split: class 0 | method 1 | signature 2
  size_t pos = 0;
  std::string str;
  std::u16string str16;
  for (size_t i = 0; i < nameLen; ) {
    unsigned char c = namePtr[i++];
    if (c == '_') { //_XX: '_' followed by ascii code in hex
      if (i >= nameLen) {
        break;
      }
      if (namePtr[i] == '_') {
       newName[pos++] = namePtr[i++];
      } else if (namePtr[i] == 'u') {
        str.clear();
        str16.clear();
        i++;
        c = namePtr[i++];
        uint8_t b1 = (c <= '9') ? c - '0' : c - 'a' + 10;
        c = namePtr[i++];
        uint8_t b2 = (c <= '9') ? c - '0' : c - 'a' + 10;
        c = namePtr[i++];
        uint8_t b3 = (c <= '9') ? c - '0' : c - 'a' + 10;
        c = namePtr[i++];
        uint8_t b4 = (c <= '9') ? c - '0' : c - 'a' + 10;
        uint32_t codepoint = b1 << 12 | b2 << 8 | b3 << 4 | b4;
        str16 += (char16_t)codepoint;
        unsigned int n = UTF16ToUTF8(str, str16, 1, false);
        if ((n >> 16) == 2) {
          newName[pos++] = str[0];
          newName[pos++] = str[1];
        } else if ((n >> 16) == 3) {
          newName[pos++] = str[0];
          newName[pos++] = str[1];
          newName[pos++] = str[2];
        } else if ((n >> 16) == 4) {
          newName[pos++] = str[0];
          newName[pos++] = str[1];
          newName[pos++] = str[2];
          newName[pos++] = str[3];
        }
      } else {
        c = namePtr[i++];
        unsigned int v = (c <= '9') ? c - '0' : c - 'A' + 10;
        unsigned int asc = v << 4;
        if (i >= nameLen) {
          break;
        }
        c = namePtr[i++];
        v = (c <= '9') ? c - '0' : c - 'A' + 10;
        asc += v;

        newName[pos++] = (char)(asc);

        if (asc == '|') {
          splitNo++;
        }

        primType = UpdatePrimType(primType, splitNo, asc);
      }
    } else {
      if(splitNo < 2) {
        newName[pos++] = c;
        continue;
      }

      primType = UpdatePrimType(primType, splitNo, c);
      if (primType) {
        newName[pos++] = (c == 'A')?'[':c;
      } else {
        newName[pos++] = c;
      }
    }
  }

  newName.resize(pos);
  return newName;
}

std::string DecodeName(const char* name) {
  std::string nameStr = std::string(name);
  return DecodeName(nameStr);
}

// input: maple name
// output: Ljava/lang/Object;  [Ljava/lang/Object;
void DecodeMapleNameToJavaDescriptor(const std::string &nameIn, std::string &nameOut) {
  nameOut = DecodeName(nameIn);
  if (nameOut[0] == 'A') {
    int i = 0;
    while(nameOut[i] == 'A') {
      nameOut[i++] = '[';
    }
  }
}

// convert maple name to java name
// http://docs.oracle.com/javase/8/docs/technotes/guides/jni/spec/design.html#resolving_native_method_names
std::string NativeJavaName(const char* name, bool overloaded) {
  // Decompress name first because the generated native function name needs
  // to follow certain spec, not something maple can control.
  std::string decompressedName(name);
  if (doCompression) {
    decompressedName = DecompressName(decompressedName);
  }

  unsigned int namelen = decompressedName.length() + 5;
  std::string newName(3*namelen, '\0');
  unsigned int pos = 0;
  unsigned int i=0;

  newName[pos++] = 'J';
  newName[pos++] = 'a';
  newName[pos++] = 'v';
  newName[pos++] = 'a';
  newName[pos++] = '_';

  // leading A's are array
  while (name[i]=='A' && i<namelen) {
    newName[pos++] = '_';
    newName[pos++] = '3';
    i++;
  }

  bool isproto = false;     // class names in prototype have 'L' and ';'
  bool isfuncname = false;
  bool istypename = false;
  while (i < namelen) {
    char c = decompressedName[i];
    if (c == '_') {
      i++;
      // UTF16 unicode
      if (decompressedName[i] == 'u') {
        newName[pos++] = '_';
        newName[pos++] = '0';
        i++;
      } else if (decompressedName[i] == '_') {
        newName[pos++] = '_';
        newName[pos++] = '1';
        i++;
      } else {
        // _XX: '_' followed by ascii code in hex
        c = decompressedName[i++];
        unsigned char v = (c <= '9') ? c - '0' : c - 'A' + 10;
        unsigned char asc = v<<4;
        c = decompressedName[i++];
        v = (c <= '9') ? c - '0' : c - 'A' + 10;
        asc += v;

        if (asc == '/') {
          newName[pos++] = '_';
        } else if (asc == '|' && !isfuncname) {
          newName[pos++] = '_';
          isfuncname = true;
        } else if (asc == '|' && isfuncname) {
          if (!overloaded) {
            break;
          }
          newName[pos++] = '_';
          isfuncname = false;
        } else if (asc == '(') {
          newName[pos++] = '_';
          isproto = true;
        } else if (asc == ')') {
          break;
        } else if (asc == ';' && !isfuncname) {
          if (isproto) {
            newName[pos++] = '_';
            newName[pos++] = '2';
          }
          istypename = false;
        } else if (asc == '$') {
          newName[pos++] = '_';
          newName[pos++] = '0';
          newName[pos++] = '0';
          newName[pos++] = '0';
          newName[pos++] = '2';
          newName[pos++] = '4';
        } else if (asc == '-') {
          newName[pos++] = '_';
          newName[pos++] = '0';
          newName[pos++] = '0';
          newName[pos++] = '0';
          newName[pos++] = '2';
          newName[pos++] = 'd';
        } else {
          printf("name = %s\n", decompressedName.c_str());
          printf("c = %c\n", asc);
          assert(0&&"TODO: more cases in NativeJavaName");
        }
      }
    } else {
      if (c == 'L' && !isfuncname && !istypename) {
        if (isproto)
          newName[pos++] = c;
        istypename = true;
        i++;
      } else if (c == 'A' && !istypename && !isfuncname) {
        while (name[i] == 'A') {
          newName[pos++] = '_';
          newName[pos++] = '3';
          i++;
        }
      } else {
        newName[pos++] = c;
        i++;
      }
    }
  }

  newName.resize(pos);
  return newName;
}

static uint16_t ChangeEndian16(uint16_t u16) {
  return ((u16 & 0xFF00) >> 8) | ((u16 & 0xFF) << 8);
}

/* UTF8
 * U+0000 - U+007F   0xxxxxxx
 * U+0080 - U+07FF   110xxxxx 10xxxxxx
 * U+0800 - U+FFFF   1110xxxx 10xxxxxx 10xxxxxx
 * U+10000- U+10FFFF 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
 *
 * UTF16
 * U+0000 - U+D7FF   codepoint
 * U+E000 - U+FFFF   codepoint
 * U+10000- U+10FFFF XXXX YYYY
 *   code = codepoint - 0x010000, ie, 20-bit number in the range 0x000000..0x0FFFFF
 *   XXXX: top 10 bits of code + 0xD800: 0xD800..0xDBFF
 *   YYYY: low 10 bits of code + 0xDC00: 0xDC00..0xDFFF
 *
 * convert upto num UTF8 elements
 * return two 16-bit values: return_number_of_elements | consumed_input_number_of_elements
 */
unsigned UTF16ToUTF8(std::string &str, const std::u16string &str16, unsigned short num, bool isbigendian) {
  uint32_t codepoint;
  uint32_t i = 0;
  unsigned short count = 0;
  unsigned short retnum = 0;
  while (i < str16.length()) {
    if (isbigendian || num == 1)
      codepoint = str16[i++];
    else
      codepoint = ChangeEndian16(str16[i++]);
    if (codepoint > 0xFFFF) {
      codepoint &= 0x3FF;
      codepoint <<= 10;
      if (isbigendian)
        codepoint += str16[i++] & 0x3FF;
      else
        codepoint += ChangeEndian16(str16[i++]) & 0x3FF;
    }
    // printf("UTF16ToUTF8 codepoint = 0x%x\n", codepoint);
    if (codepoint <= 0x7F) {
      str += (uint8_t)codepoint;
      retnum += 1;
    } else if (codepoint <= 0x7FF) {
      str += (uint8_t)(0xC0 + (codepoint >> 6));
      str += (uint8_t)(0x80 + (codepoint & 0x3F));
      retnum += 2;
    } else if (codepoint <= 0xFFFF) {
      str += (uint8_t)(0xE0 + ((codepoint >> 12) & 0xF));
      str += (uint8_t)(0x80 + ((codepoint >> 6) & 0x3F));
      str += (uint8_t)(0x80 + (codepoint & 0x3F));
      retnum += 3;
    } else {
      str += (uint8_t)(0xF0 + ((codepoint >> 18) & 0x7));
      str += (uint8_t)(0x80 + ((codepoint >> 12) & 0x3F));
      str += (uint8_t)(0x80 + ((codepoint >> 6) & 0x3F));
      str += (uint8_t)(0x80 + (codepoint & 0x3F));
      retnum += 4;
    }

    count++;

    if (num == count)
      return (((unsigned)retnum) << 16) | (unsigned)i;
  }
  return i;
}

// convert upto num UTF16 elements
// two 16-bit values: return_number_of_elements | consumed_input_number_of_elements
unsigned UTF8ToUTF16(std::u16string &str16, const std::string &str8, unsigned short num, bool isbigendian) {
  uint32_t a;
  uint32_t b;
  uint32_t c;
  uint32_t d;
  uint32_t codepoint;
  uint32_t i = 0;
  unsigned short count = 0;
  unsigned short retnum = 0;
  while (i < str8.length()) {
    a = (uint8_t)(str8[i++]);
    if (a <= 0x7F) { // 0...
      codepoint = a;
    } else if (a >= 0xF0) { // 11110...
      b = str8[i++];
      c = str8[i++];
      d = str8[i++];
      codepoint = ((a & 0x7) << 18) | ((b & 0x3F) << 12) | ((c & 0x3F) << 6) | (d & 0x3F);
    } else if (a >= 0xE0) { // 1110...
      b = str8[i++];
      c = str8[i++];
      codepoint = ((a & 0xF) << 12) | ((b & 0x3F) << 6) | (c & 0x3F);
    } else if (a >= 0xC0) { // 110...
      b = str8[i++];
      codepoint = ((a & 0x1F) << 6) | (b & 0x3F);
    } else {
      assert(false&&"invalid UTF-8");
    }
    // printf("UTF8ToUTF16 codepoint = 0x%x\n", codepoint);
    if (codepoint <= 0xFFFF) {
      if (isbigendian || num == 1)
        str16 += (char16_t)codepoint;
      else
        str16 += (char16_t)ChangeEndian16(codepoint);
      retnum += 1;
    } else {
      codepoint -= 0x10000;
      if (isbigendian || num == 1) {
        str16 += (char16_t)((codepoint >> 10) | 0xD800);
        str16 += (char16_t)((codepoint & 0x3FF) | 0xDC00);
      } else {
        str16 += (char16_t)ChangeEndian16((codepoint >> 10) | 0xD800);
        str16 += (char16_t)ChangeEndian16((codepoint & 0x3FF) | 0xDC00);
      }
      retnum += 2;
    }

    count++;

    // only convert num elmements
    if (num == count)
      return (((unsigned)retnum) << 16) | (unsigned)i;
  }
  return i;
}

uint64_t GetLEB128Encode(int64_t val, bool isUnsigned) {
  uint64_t res = 0;
  uint8_t byte;
  uint8_t count = 0;
  bool done = false;
  do {
    byte = (uint64_t)(val) & 0x7f;
    val >>= 7;
    done = ( isUnsigned ? val == 0 : ( val == 0 || val == -1 ) );
    if (!done)
      byte |= 0x80;
    res |= ((uint64_t)(byte) << (count++ << 3));
  } while (!done);
  return res;
}

uint64_t GetUleb128Encode(uint64_t val) {
  return GetLEB128Encode(int64_t(val),true);
}

uint64_t GetSleb128Encode(int64_t val) {
  return GetLEB128Encode(val,false);
}

uint64_t GetUleb128Decode(uint64_t val) {
  return val;
}

int64_t GetSleb128Decode(uint64_t val) {
  return val;
}

size_t GetUleb128Size(uint64_t v) {
  assert( v&& "" );
  // if v == 0, __builtin_clzll(v) is not defined
  int clz = __builtin_clzll (v);
  // num of 7-bit groups
  return size_t((64-clz+6)/7);
}

size_t GetSleb128Size(int32_t v) {
  size_t size = 0;
  int rem = v >> 7;
  bool hasMore = true;
  int end = ((v >= 0) ? 0 : -1);

  while (hasMore) {
    hasMore = (rem != end) || ((rem & 1) != ((v >> 6) & 1));
    size++;
    v = rem;
    rem >>= 7;
  }
  return size;
}


} // namespace NameMangler
