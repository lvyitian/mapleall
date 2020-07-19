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
open(my $aarch64_isa.def_h, '<',  "aarch64isa.def.in") or die "can't open aarch64isa.def.in";
open(my $aarch64_isa.def, '>',  "aarch64isa.def") or die "can't open aarch64isa.def";
print $aarch64_isa.def "\/\/Do not modify this file manually\n";
my $insn_count=1;
while(my $line = <$aarch64_isa.def_h>) {
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
  print $aarch64_isa.def $line;
}
print $aarch64_isa.def "\#define kMopLast ".$insn_count."\n";
close $aarch64_isa.def;
close $aarch64_isa.def_h;
