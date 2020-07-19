#
# Copyright (c) [2020] Huawei Technologies Co., Ltd. All rights reserved.
#
# Licensed under the Mulan Permissive Software License v2.
# You can use this software according to the terms and conditions of the MulanPSL - 2.0.
# You may obtain a copy of MulanPSL - 2.0 at:
#
#     https://opensource.org/licenses/MulanPSL-2.0
#
# THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND, EITHER
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT, MERCHANTABILITY OR
# FIT FOR A PARTICULAR PURPOSE.
# See the MulanPSL - 2.0 for more details.
#
open(my $armisa_def_h, '<',  "armisa.def.h") or die "can't open armisa.def.h";
open(my $armisa_def, '>',  "armisa.def") or die "can't open armisa.def";
my $insn_count=1;
while(my $line = <$armisa_def_h>) {
  if ($line =~ /\/\//){
    next;
  }
  elsif ($line =~ /( )*MOP_/){
    $line =~ s/( )*MOP_/\#define MOP_/;
    $line =~ s/,/   $insn_count/;
    $insn_count++;
  }else {
    next;
  }
  print $armisa_def $line;
}
print $armisa_def "\#define kMopLast ".$insn_count."\n";
close $armisa_def;
close $armisa_def_h;
