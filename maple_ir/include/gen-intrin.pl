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

open(my $intrin_def, '<',  "intrinsicsop.def.h") or die "can't open intrinsics.def";
open(my $intrin_h, '>',  "intrinsicsop.def") or die "can't open intrinsics.h";
open(my $intrinjs_def, '<',  "./js2mpl/intrinsicsop.def.h") or die "can't open intrinsics.def";
open(my $intrinjs_h, '>',  "./js2mpl/intrinsicsop.def") or die "can't open intrinsics.h";
my $insn_count=1;
while(my $line = <$intrin_def>) {
  chomp $line;
  print $intrin_h "\#define INTRINSIC_".$line." ".$insn_count."\n";
  $insn_count++;
}
while(my $line = <$intrinjs_def>) {
  chomp $line;
  print $intrin_h "\#define INTRINSIC_".$line." ".$insn_count."\n";
  $insn_count++;
}
print $intrin_h "\#define INTRINSIC_last ".$insn_count."\n";
close $intrin_def;
close $intrin_h;
close $intrinjs_def;
close $intrinjs_h;
