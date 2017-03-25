use strict;
use warnings;
use utf8;
binmode(STDOUT, ":utf8");


my $str = 'りんご';
print 'これは'.$str."です。\n";
print "これは${str}です。\n";

my $name = "KENT";
print "my name is $name", "\n";
print 'my name is $name', "\n";

# my $line = <STDIN>;
# print "入力された値 : ${line}";

my $word = "Hello\n";
# $word <- "Hello"
print(chomp($word)); # => 1 (<-!!!!!)
print "\n";
print $word
