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
open(my $x86isa_def_h, '<',  "x86isa.def.h") or die "can't open x86isa.def.h";
open(my $x86isa_def, '>',  "x86isa.def") or die "can't open x86isa.def";
my $insn_count=1;
while(my $line = <$x86isa_def_h>) {
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
  print $x86isa_def $line;
}
print $x86isa_def "\#define kMopLast ".$insn_count."\n";
close $x86isa_def;
close $x86isa_def_h;
