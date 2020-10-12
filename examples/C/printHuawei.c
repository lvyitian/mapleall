/*
 * Copyright (C) [2020] Futurewei Technologies, Inc. All rights reverved.
 *
 * Licensed under the Mulan Permissive Software License v2.
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

#include <stdio.h>

int main() {
  int width = 142;
  char *str = "\
     F efifdfifkehfhfhfgnefb;efifdfifjghffhffgoefb; \
     U efifdfifiigffhfffpefb;efifdfifiigfeiffeqefb; \
     T efifdfifhkgeejeeegpfb;efifdfifhkgfdjdfefqfb; \
     U efifdfifgfafffckdfdfrfb;eudfifgfafgeceafceesefb; \
     R eudfifffcfffbebebfesefb;eudfifffcfffafbfaefsefb; \
     E eudfifefefefaecfaefsefb;efifdfifeqfeaedeaeffrfb; \
     W efifdfifdsekdkffrfb;efifdfifdsejfjfgqfb; \
     E efifdfifcueifihfqfb;efifefgfdfifeifihhofb; \
     I efifescfkfdhhhiqefb;efiffqdfkfdhhhjpefb; \
       efifgodfmfdghgmnefb;efifikffmfdfjfolefb;";
  putchar('\n');
  char c;
  int idx = 0;
  int loc = 0;
  while ((c = str[idx++])) {
    char fillChar = (idx & 1) ? 'H' : ' ';
    for(char b = 'a'; b <= c; b++) {
      if (loc++ == width) {
        loc = 0;
        fillChar = '\n';
      }
      putchar(fillChar);
    }
  }
  putchar('\n');
  return 0;
}
