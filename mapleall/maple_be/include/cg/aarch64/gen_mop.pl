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
open(my $aarch64isa_def_h, '<',  "aarch64_isa.def.in") or die "can't open aarch64_isa.def.in";
open(my $aarch64isa_def, '>',  "aarch64_isa.def") or die "can't open aarch64_isa.def";
print $aarch64isa_def "\/\/Do not modify this file manually\n";
my $insn_count=1;
while(my $line = <$aarch64isa_def_h>) {
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
  print $aarch64isa_def $line;
}
print $aarch64isa_def "\#define kMopLast ".$insn_count."\n";
close $aarch64isa_def;
close $aarch64isa_def_h;
