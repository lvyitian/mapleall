/*
 * Copyright (c) [2020] Huawei Technologies Co., Ltd. All rights reserved.
 *
 * OpenArkCompiler is licensed under the Mulan Permissive Software License v2.
 * You can use this software according to the terms and conditions of the MulanPSL - 2.0.
 * You may obtain a copy of MulanPSL - 2.0 at:
 *
 *   https://opensource.org/licenses/MulanPSL-2.0
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND, EITHER
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT, MERCHANTABILITY OR
 * FIT FOR A PARTICULAR PURPOSE.
 * See the MulanPSL - 2.0 for more details.
 */

#ifndef MAPLE_UTIL_INCLUDE_NAME_MANGLER_H
#define MAPLE_UTIL_INCLUDE_NAME_MANGLER_H
#include <string>
#include <vector>

// This is a general name mangler which is shared between maple compiler and runtime.
// maple-compiler-specific data structure may NOT be used here.
namespace NameMangler {

#define TO_STR(s)  TO_STR2(s)
#define TO_STR2(s) #s


#define VTAB_PREFIX               __vtb_
#define ITAB_PREFIX               __itb_
#define ITAB_CONFLICT_PREFIX      __itabC_
#define CLASSINFO_PREFIX          __cinf_
#define CLASSINFO_RO_PREFIX       __classinforo__
#define METHODINFO_RO_PREFIX      __methodsro__
#define FIELDINFO_RO_PREFIX       __fieldsro__
#define SUPERCLASSINFO_PREFIX     __superclasses__
#define PRIMITIVECLASSINFO_PREFIX __pinf_
#define CLASS_INIT_BRIDGE_PREFIX  __ClassInitBridge__
#define GCTIB_PREFIX              MCC_GCTIB__
#define REF_PREFIX                REF_
#define JARRAY_PREFIX             A

#define VTAB_PREFIX_STR               TO_STR(VTAB_PREFIX)
#define ITAB_PREFIX_STR               TO_STR(ITAB_PREFIX)
#define ITAB_CONFLICT_PREFIX_STR      TO_STR(ITAB_CONFLICT_PREFIX)
#define CLASSINFO_PREFIX_STR          TO_STR(CLASSINFO_PREFIX)
#define CLASSINFO_RO_PREFIX_STR       TO_STR(CLASSINFO_RO_PREFIX)
#define METHODINFO_RO_PREFIX_STR      TO_STR(METHODINFO_RO_PREFIX)
#define FIELDINFO_RO_PREFIX_STR       TO_STR(FIELDINFO_RO_PREFIX)
#define SUPERCLASSINFO_PREFIX_STR     TO_STR(SUPERCLASSINFO_PREFIX)
#define PRIMITIVECLASSINFO_PREFIX_STR TO_STR(PRIMITIVECLASSINFO_PREFIX)
#define CLASS_INIT_BRIDGE_PREFIX_STR  TO_STR(CLASS_INIT_BRIDGE_PREFIX)
#define GCTIB_PREFIX_STR              TO_STR(GCTIB_PREFIX)
#define REF_PREFIX_STR                TO_STR(REF_PREFIX)
#define JARRAY_PREFIX_STR             TO_STR(JARRAY_PREFIX)

// Names of all compiler-generated tables and accessed by runtime
#define MUID_FIELDINFO_PREFIX_STR             "__muid_fieldinfo"
static constexpr const char kMuidPrefixStr[] = "__muid_";
static constexpr const char kMuidRoPrefixStr[] = "__muid_ro";
static constexpr const char kMuidFuncDefTabPrefixStr[] = "__muid_func_def_tab";
static constexpr const char kMuidFuncDefOrigTabPrefixStr[] = "__muid_ro_func_def_orig_tab";
static constexpr const char kMuidFuncInfTabPrefixStr[] = "__muid_func_inf_tab";
static constexpr const char kMuidFuncMuidIdxTabPrefixStr[] = "__muid_func_muid_idx_tab";
static constexpr const char kMuidDataDefTabPrefixStr[] = "__muid_data_def_tab";
static constexpr const char kMuidDataDefOrigTabPrefixStr[] = "__muid_ro_data_def_orig_tab";
static constexpr const char kMuidFuncUndefTabPrefixStr[] = "__muid_func_undef_tab";
static constexpr const char kMuidDataUndefTabPrefixStr[] = "__muid_data_undef_tab";
static constexpr const char kMuidFuncDefMuidTabPrefixStr[] = "__muid_func_def_muid_tab";
static constexpr const char kMuidDataDefMuidTabPrefixStr[] = "__muid_ro_data_def_muid_tab";
static constexpr const char kMuidFuncUndefMuidTabPrefixStr[] = "__muid_func_undef_muid_tab";
static constexpr const char kMuidDataUndefMuidTabPrefixStr[] = "__muid_ro_data_undef_muid_tab";
static constexpr const char kMuidVtabPrefixStr[] = "__muid_vtab";
static constexpr const char kMuidItabPrefixStr[] = "__muid_itab";
static constexpr const char kMuidVtabOffsetPrefixStr[] = "__muid_vtab_offset_tab";
static constexpr const char kMuidFieldOffsetPrefixStr[] = "__muid_field_offset_tab";
static constexpr const char kMuidVtabOffsetKeyPrefixStr[] = "__muid_vtable_offset_key_tab";
static constexpr const char kMuidFieldOffsetKeyPrefixStr[] = "__muid_field_offset_key_tab";
static constexpr const char kMuidValueOffsetPrefixStr[] = "__muid_offset_value_table";
static constexpr const char kMuidLocalClassInfoStr[] = "__muid_local_classinfo_tab";
static constexpr const char kMuidSuperclassPrefixStr[] = "__muid_superclass";
static constexpr const char kMuidGlobalRootlistPrefixStr[] = "__muid_globalrootlist";
static constexpr const char kMuidClassMetadataPrefixStr[] = "__muid_classmetadata";
static constexpr const char kMuidClassMetadataBucketPrefixStr[] = "__muid_classmetadata_bucket";
static constexpr const char kMuidJavatextPrefixStr[] = "__muid_java_text";
static constexpr const char kMuidDataSectionStr[] = "__data_section";
static constexpr const char kMuidRangeTabPrefixStr[] = "__muid_range_tab";
static constexpr const char kMuidConststrPrefixStr[] = "__muid_conststr";
static constexpr const char kVtabOffsetTabStr[] = "__vtable_offset_table";
static constexpr const char kFieldOffsetTabKeyStr[] = "__field_offset_key_table";
static constexpr const char kFieldOffsetTabStr[] = "__field_offset_table";
static constexpr const char kVtableKeyOffsetTabStr[] = "__vtable_offset_key_table";
static constexpr const char kVtableOffsetTabKeyStr[] = "__vtable_offset_key_table";
static constexpr const char kFieldKeyOffsetTabStr[] = "__field_offset_table";
static constexpr const char kOffsetTabStr[] = "__offset_value_table";
static constexpr const char kLocalClassInfoStr[] = "__local_classinfo_table";
static constexpr const char kMethodsInfoPrefixStr[] = "__methods__";
static constexpr const char kMethodsInfoCompactPrefixStr[] = "__methods_compact__";
static constexpr const char kFieldsInfoPrefixStr[] = "__fields__";
static constexpr const char kFieldsInfoCompactPrefixStr[] = "__fields_compact__";
static constexpr const char kFieldOffsetDataPrefixStr[] = "__fieldOffsetData__";
static constexpr const char kMethodAddrDataPrefixStr[] = "__methodAddrData__";
static constexpr const char kRegJNITabPrefixStr[] = "__reg_jni_tab";
static constexpr const char kRegJNIFuncTabPrefixStr[] = "__reg_jni_func_tab";
static constexpr const char kReflectionStrtabPrefixStr[] = "__reflection_strtab";
static constexpr const char kReflectionStartHotStrtabPrefixStr[] = "__reflection_start_hot_strtab";
static constexpr const char kReflectionBothHotStrTabPrefixStr[] = "__reflection_both_hot_strtab";
static constexpr const char kReflectionRunHotStrtabPrefixStr[] = "__reflection_run_hot_strtab";
static constexpr const char kReflectionNoEmitStrtabPrefixStr[] = "__reflection_no_emit_strtab";
static constexpr const char kMarkMuidFuncDefStr[] = "muid_func_def:";
static constexpr const char kMarkMuidFuncUndefStr[] = "muid_func_undef:";
static constexpr const char kGcRootList[] = "MRT_GCRootNewList";
static constexpr const char kArrayObject[] = "ArrayObject";
static constexpr const char kDecoupleOption[] = "__decouple_option";
static constexpr const char kDecoupleStr[] = "__decouple";
static constexpr const char kCompilerVersionNum[] = "__compilerVersionNum";
static constexpr const char kCompilerVersionNumStr[] = "__compilerVersionNumTab";
static constexpr const char kCompilerMfileStatus[]  = "__compiler_mfile_status";

static constexpr const char kSourceMuid[] = "__sourceMuid";
static constexpr const char kSourceMuidSectionStr[] = "__sourceMuidTab";
static constexpr const char kDecoupleStaticKeyStr[] = "__staticDecoupleKeyOffset";
static constexpr const char kDecoupleStaticValueStr[] = "__staticDecoupleValueOffset";
static constexpr const char kMarkDecoupleStaticStr[] = "decouple_static:";
static constexpr const char kClassInfoPrefix[] = "__cinf";
static constexpr const char kBssSectionStr[] = "__bss_section";
static constexpr const char kLinkerHashSoStr[] = "__linkerHashSo";

static constexpr const char kStaticFieldNamePrefixStr[] = "__static_field_name";
static constexpr const char kMplSuffix[] = ".mpl";
static constexpr const char kClinvocation[] = ".clinvocation";
static constexpr const char kPackageNameSplitterStr[] = "_2F";
static constexpr const char kFileNameSplitterStr[] = "$$";
static constexpr const char kNameSplitterStr[] = "_7C";  // 7C is the ascii code for |
static constexpr const char kRightBracketStr[] = "_29";  // 29 is the ascii code for )
static constexpr const char kRightBracketStrSuffix[] = "_29V";
static constexpr const char kClassNameSplitterStr[] = "_3B_7C";
static constexpr const char kJavaLangClassStr[] = "Ljava_2Flang_2FClass_3B";
static constexpr const char kJavaLangInvoke[] = "Ljava_2Flang_2Finvoke_2F";
static constexpr const char kJavaLangReflect[] = "Ljava_2Flang_2Freflect_2F";
static constexpr const char kJavaLangObjectStr[] = "Ljava_2Flang_2FObject_3B";
static constexpr const char kJavaLangRef[] = "Ljava_2Flang_2Fref_2F";
static constexpr const char kJavaLangClassloader[] = "Ljava_2Flang_2FClassLoader_3B";
static constexpr const char kJavaLangObjectStrJVersion[] = "Ljava/lang/Object;";
static constexpr const char kJavaLangStringStr[] = "Ljava_2Flang_2FString_3B";
static constexpr const char kJavaLangStringfactoryStr[] = "Ljava_2Flang_2FStringFactory_3B";
static constexpr const char kJavaLangExceptionStr[] = "Ljava_2Flang_2FException_3B";
static constexpr const char kJavaUtilFormatterFlags[] = "Ljava_2Futil_2FFormatter_24Flags_3B";
static constexpr const char kJavaUtilHashMapNode[] = "Ljava_2Futil_2FHashMap_24Node_3B";
static constexpr const char kJavaUtilFormatterFormatString[] = "Ljava_2Futil_2FFormatter_24FormatString_3B";
static constexpr const char kJavaUtilHashTableHashTableEntry[] = "Ljava_2Futil_2FHashtable_24HashtableEntry_3B";
static constexpr const char kThrowClassStr[] = "Ljava_2Flang_2FThrowable_3B";
static constexpr const char kReflectionClassesPrefixStr[] = "Ljava_2Flang_2Freflect_2F";
static constexpr const char kClassMetadataTypeName[] = "__class_meta__";
static constexpr const char kPtrPrefixStr[] = "_PTR";
static constexpr const char kClassINfoPtrPrefixStr[] = "_PTR__cinf_";
static constexpr const char kArrayClassInfoPrefixStr[] = "__cinf_A";
static constexpr const char kShadowClassName[] = "shadow_24__klass__";
static constexpr const char kInitSuffix[] = "_7C_3Cinit_3E_7C_28_29V";
static constexpr const char kClinitSuffix[] = "_7C_3Cclinit_3E_7C_28_29V";
static constexpr const char kGcSuffix[] = "_7Cgc_7C_28_29V";
static constexpr const char kCinitStr[] = "_7C_3Cinit_3E_7C_28";
static constexpr const char kClinitSubStr[] = "7C_3Cinit_3E_7C";
static constexpr const char kHashCodeStr[] = "_7ChashCode_7C_28_29I";
static constexpr const char kContainerHelper[] = "ContainerHelpers_3B";
static constexpr const char kParcelNativeWriteString[] = "Parcel_3B_7CnativeWriteString_7C_28J";
static constexpr const char kReference[] = "Reference_3B";
static constexpr const char kReferenceQueue[] = "ReferenceQueue_3B_29V";
static constexpr const char kSparseArray[] = "SparseArray_3B";

static constexpr const char kFunctionLayoutStr[] = "__func_layout__";
static constexpr const char kJavaLangBoolean[] = "Ljava_2Flang_2FBoolean_3B";
static constexpr const char kJavaLangBooleanTrue[] = "Ljava_2Flang_2FBoolean_3B_7CTRUE";
static constexpr const char kJavaLangBooleanFalse[] = "Ljava_2Flang_2FBoolean_3B_7CFALSE";
static constexpr const char kFunctionProfileTabPrefixStr[] = "__muid_profile_func_tab";

static constexpr const char kJavaLangReflectAccessibleObject[] = "Ljava_2Flang_2Freflect_2FAccessibleObject_3B";
static constexpr const char kJavaLangReflectConstructor[] = "Ljava_2Flang_2Freflect_2FConstructor_3B";
static constexpr const char kJavaLangReflectExecutable[] = "Ljava_2Flang_2Freflect_2FExecutable_3B";
static constexpr const char kJavaLangReflectField[] = "Ljava_2Flang_2Freflect_2FField_3B";
static constexpr const char kJavaLangReflectMember[] = "Ljava_2Flang_2Freflect_2FMember_3B";
static constexpr const char kJavaLangReflectMethod[] = "Ljava_2Flang_2Freflect_2FMethod_3B";
static constexpr const char kJavaLangReflectType[] = "Ljava_2Flang_2Freflect_2FType_3B";
static constexpr const char kJavaLangReflectAnnotateElement[] = "Ljava_2Flang_2Freflect_2FAnnotatedElement_3B";
static constexpr const char kJavaLangReflectGenericDeclaration[] = "Ljava_2Flang_2Freflect_2FGenericDeclaration_3B";
static constexpr const char kJavaLangThreadLocalMapEntry[] = "Ljava_2Flang_2FThreadLocal_24ThreadLocalMap_24Entry_3B";

static constexpr const char kJavaLangCharSequence[] = "Ljava_2Flang_2FCharSequence_3B";
static constexpr const char kJavaLangComparable[] = "Ljava_2Flang_2FComparable_3B";

static constexpr const char kJavaIoSerializable[] = "Ljava_2Fio_2FSerializable_3B";

static constexpr const char kBindingProtectedRegionStr[] = "__BindingProtectRegion__";

static constexpr const char kClassNamePrefixStr[] = "L";
static constexpr const char kClassMethodSplitterStr[] = "_3B";
static constexpr const char kFuncGetCurrentCl[] = "MCC_GetCurrentClassLoader";

static constexpr const char kJavaLang[] = "Ljava_2Flang_2F";
static constexpr const char kByte[] = "Byte_3B";
static constexpr const char kCharacter[] = "Character_3B";
static constexpr const char kShort[] = "Short_3B";
static constexpr const char kInteger[] = "Integer_3B";
static constexpr const char kLong[] = "Long_3B";
static constexpr const char kFloat[] = "Float_3B";
static constexpr const char kDouble[] = "Double_3B";
static constexpr const char kCloneable[] = "Cloneable_3B";
static constexpr const char kEnum[] = "Enum_3B";
static constexpr const char kReflectProxy[] = "reflect_2FProxy_3B";
static constexpr const char kReflectParameter[] = "reflect_2FParameter_3B";
static constexpr const char kError[] = "Error_3B";
static constexpr const char kNoClassDefFoundError[] = "NoClassDefFoundError_3B";
static constexpr const char kExceptionInInitializerError[] = "ExceptionInInitializerError_3B";
static constexpr const char kRuntimeException[] = "RuntimeException_3B";
static constexpr const char kReflectUndeclaredThrowableException[] = "reflect_2FUndeclaredThrowableException_3B";
static constexpr const char kArrayStoreException[] = "ArrayStoreException_3B";
static constexpr const char kArrayIndexOutOfBoundsException[] = "ArrayIndexOutOfBoundsException_3B";
static constexpr const char kNullPointerException[] = "NullPointerException_3B";
static constexpr const char kArithmeticException[] = "ArithmeticException_3B";
static constexpr const char kInterruptedException[] = "InterruptedException_3B";
static constexpr const char kClassCastException[] = "ClassCastException_3B";
static constexpr const char kUnsatisfiedLinkError[] = "UnsatisfiedLinkError_3B";
static constexpr const char kStringIndexOutOfBoundsException[] = "StringIndexOutOfBoundsException_3B";
static constexpr const char kSecurityException[] = "SecurityException_3B";
static constexpr const char kAnnotationAnnotation[] = "annotation_2FAnnotation_3B";
static constexpr const char kRefReference[] = "ref_2FReference_3B";
static constexpr const char kRefWeakReference[] = "ref_2FWeakReference_3B";
static constexpr const char kRefFinalizeReference[] = "ref_2FFinalizerReference_3B";

static constexpr const char kJavaIo[] = "Ljava_2Fio_2F";
static constexpr const char kJavaNio[] = "Ljava_2Fnio_2F";
static constexpr const char kJavaNioChannels[] = "Ljava_2Fnio_2Fchannels_2F";
static constexpr const char kJavaNioChannelsSpi[] = "Ljava_2Fnio_2Fchannels_2Fspi_2F";
static constexpr const char kJavaNioCharset[] = "Ljava_2Fnio_2Fcharset_2F";
static constexpr const char kJavaNioFile[] = "Ljava_2Fnio_2Ffile_2F";
static constexpr const char kJavaNet[] = "Ljava_2Fnet_2F";
static constexpr const char kJavaMath[] = "Ljava_2Fmath_2F";
static constexpr const char kJavaSecurity[] = "Ljava_2Fsecurity_2F";
static constexpr const char kJavaSecurityCert[] = "Ljava_2Fsecurity_2Fcert_2F";
static constexpr const char kJavaSecuritySpec[] = "Ljava_2Fsecurity_2Fspec_2F";
static constexpr const char kJavaText[] = "Ljava_2Ftext_2F";
static constexpr const char kSerializable[] = "Serializable_3B";
static constexpr const char kFile[] = "File_3B";

static constexpr const char kJavaUtil[] = "Ljava_2Futil_2F";
static constexpr const char kJavaUtilConcurrent[] = "Ljava_2Futil_2Fconcurrent_2F";
static constexpr const char kJavaUtilConcurrentAtomic[] = "Ljava_2Futil_2Fconcurrent_2Fatomic_2F";
static constexpr const char kJavaUtilConcurrentLocks[] = "Ljava_2Futil_2Fconcurrent_2Flocks_2F";
static constexpr const char kJavaUtilJar[] = "Ljava_2Futil_2Fjar_2F";
static constexpr const char kJavaUtilLogging[] = "Ljava_2Futil_2Flogging_2F";
static constexpr const char kJavaUtilPrefs[] = "Ljava_2Futil_2Fprefs_2F";
static constexpr const char kJavaUtilRegex[] = "Ljava_2Futil_2Fregex_2F";
static constexpr const char kJavaUtilStream[] = "Ljava_2Futil_2Fstream_2F";
static constexpr const char kJavaUtilUtil[] = "Ljava_2Futil_2Futil_2F";
static constexpr const char kJavaUtilZip[] = "Ljava_2Futil_2Fzip_2F";

static constexpr const char kSunInvoke[] = "Lsun_2Finvoke_2F";
static constexpr const char kSunInvokeUtil[] = "Lsun_2Finvoke_2Futil_2F";
static constexpr const char kSunMisc[] = "Lsun_2Fmisc_2F";
static constexpr const char kSunNet[] = "Lsun_2Fnet_2F";
static constexpr const char kSunNetSpi[] = "Lsun_2Fnet_2Fspi_2F";
static constexpr const char kSunNetWww[] = "Lsun_2Fnet_2Fwww_2F";
static constexpr const char kSunNetWwwProtocol[] = "Lsun_2Fnet_2Fwww_2Fprotocol_2F";
static constexpr const char kSunNetWwwProtocolFile[] = "Lsun_2Fnet_2Fwww_2Fprotocol_2Ffile_2F";
static constexpr const char kSunNio[] = "Lsun_2Fnio_2F";
static constexpr const char kSunNioCh[] = "Lsun_2Fnio_2Fch_2F";
static constexpr const char kSunNioCs[] = "Lsun_2Fnio_2Fcs_2F";
static constexpr const char kSunNioFs[] = "Lsun_2Fnio_2Ffs_2F";
static constexpr const char kSunSecurity[] = "Lsun_2Fsecurity_2F";
static constexpr const char kSunSecurityJca[] = "Lsun_2Fsecurity_2Fjca_2F";
static constexpr const char kSunSecurityPkcs[] = "Lsun_2Fsecurity_2Fpkcs_2F";
static constexpr const char kSunSecurityProvider[] = "Lsun_2Fsecurity_2Fprovider_2F";
static constexpr const char kSunSecurityProviderCertpath[] = "Lsun_2Fsecurity_2Fprovider_2Fcertpath_2F";
static constexpr const char kSunSecurityUtil[] = "Lsun_2Fsecurity_2Futil_2F";
static constexpr const char kSunSecurityX509[] = "Lsun_2Fsecurity_2Fx509_2F";
static constexpr const char kSunUtil[] = "Lsun_2Futil_2F";
static constexpr const char kSunUtilCalendar[] = "Lsun_2Futil_2Flocale_2Fcalendar_2F";
static constexpr const char kSunUtilLocale[] = "Lsun_2Futil_2Flocale_2F";
static constexpr const char kSunUtilLogging[] = "Lsun_2Futil_2Flogging_2F";

static constexpr const char kLibcoreIcu[] = "Llibcore_2Ficu_2F";
static constexpr const char kLibcoreIo[] = "Llibcore_2Fio_2F";
static constexpr const char kLibcoreMath[] = "Llibcore_2Fmath_2F";
static constexpr const char kLibcoreNet[] = "Llibcore_2Fnet_2F";
static constexpr const char kLibcoreNetEvent[] = "Llibcore_2Fnet_2Fevent_2F";
static constexpr const char kLibcoreReflect[] = "Llibcore_2Freflect_2F";
static constexpr const char kLibcoreUtil[] = "Llibcore_2Futil_2F";

static constexpr const char kOrgApacheHarmonyXml[] = "Lorg_2Fapache_2Fharmony_2Fxml_2F";
static constexpr const char kOrgApacheHarmonyXmlDom[] = "Lorg_2Fapache_2Fharmony_2Fxml_2Fdom_2F";
static constexpr const char kOrgApacheHarmonyXmlParsers[] = "Lorg_2Fapache_2Fharmony_2Fxml_2Fparsers_2F";
static constexpr const char kOrgJson[] = "Lorg_2Fjson_2F";

static constexpr const char kJavaxCrypto[] = "Ljavax_2Fcrypto_2F";
static constexpr const char kJavaxNet[] = "Ljavax_2Fnet_2F";
static constexpr const char kJavaxNetSsl[] = "Ljavax_2Fnet_2Fssl_2F";
static constexpr const char kJavaxSecurity[] = "Ljavax_2Fsecurity_2F";
static constexpr const char kJavaxSecurityCert[] = "Ljavax_2Fsecurity_2Fcert_2F";
static constexpr const char kJavaxXml[] = "Ljavax_2Fxml_2F";
static constexpr const char kJavaxXmlParsers[] = "Ljavax_2Fxml_2Fparsers_2F";

static constexpr const char kHashMapNode[] = "HashMap_24Node_3B";
static constexpr const char kFormatterFormatString[] = "Formatter_24FormatString_3B";
static constexpr const char kHashtableHashtableEntry[] = "Hashtable_24HashtableEntry_3B";
static constexpr const char kHashtableEntry[] = "Hashtable_24Entry_3B";
static constexpr const char kSimpleTimeZone[] = "SimpleTimeZone_3B";

// Serve as a global flag to indicate whether frequent strings have been compressed
extern bool doCompression;

// Return the input string if the compression is not on; otherwise, return its compressed version
std::string GetInternalNameLiteral(const char *name);
std::string GetOriginalNameLiteral(const char *name);

std::string EncodeName(const std::string &name);
std::string EncodeName(const char *name);
std::string DecodeName(const std::string &name);
std::string DecodeName(const char *name);
void DecodeMapleNameToJavaDescriptor(const std::string &nameIn, std::string &nameOut);

std::string NativeJavaName(const char *name, bool overLoaded = true);

__attribute__((visibility("default"))) unsigned UTF16ToUTF8(std::string &str, const std::u16string &str16,
                                                            unsigned short num = 0, bool isBigEndian = false);
__attribute__((visibility("default"))) unsigned UTF8ToUTF16(std::u16string &str16, const std::string &str,
                                                            unsigned short num = 0, bool isBigEndian = false);
void GetUnsignedLeb128Encode(std::vector<uint8_t> &dest, uint32_t value);
uint32_t GetUnsignedLeb128Decode(const uint8_t **data);
uint64_t GetUleb128Encode(uint64_t val);
uint64_t GetSleb128Encode(int64_t val);
uint64_t GetUleb128Decode(uint64_t val);
int64_t  GetSleb128Decode(uint64_t val);
size_t GetUleb128Size(uint64_t val);
size_t GetSleb128Size(int32_t val);
bool NeedConvertUTF16(const std::string &str8);

} // namespace NameMangler

#endif
