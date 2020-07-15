#
# Copyright (c) [2020] Huawei Technologies Co.,Ltd.All rights reserved.
#
# OpenArkCompiler is licensed under the Mulan PSL v1.
# You can use this software according to the terms and conditions of the Mulan PSL v1.
# You may obtain a copy of Mulan PSL v1 at:
#
#     http://license.coscl.org.cn/MulanPSL
#
# THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND, EITHER
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT, MERCHANTABILITY OR
# FIT FOR A PARTICULAR PURPOSE.
# See the Mulan PSL v1 for more details.
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
