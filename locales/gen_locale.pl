#!/usr/bin/perl

$lang=$ARGV[0];
print "Generate $lang locale ...";
@files=`ls ../*.ml`;
$ofile="$lang.ml";

print "\n";

open(OF, $ofile);
$n=0;
while ($l=<OF>) {
  if ($l=~/^.*\".*\".*$/) {  
    if ($n==0) {
      $l=~s/^(.*)(\".*\")(.*)\n$/$2/;
      if ((!($l eq "\"$lang\"")) and (!($l eq "\"\"")))
	{
	  $key=$l;
	  $n=1;
	}
    }
    else {
      $l=~s/^(.*)(\".*\")(.*)\n$/$2/;
      $locale{$key}=$l;
      $n=0;
    }
  }
}
close(OF);



open(OF, ">$ofile");

print OF "open Locale;;\n\n";
print OF "Hashtbl.add locales \"$lang\" (new locale \"$lang\");\n";
print OF "let loc e t=(Hashtbl.find locales \"$lang\")#add e t in\n";
print OF "\n";

foreach $filename (@files)
  {
    $filename=~s/\n//;
    # print $filename . "\n";
    open(FH,$filename);
    $n=0;
    while ($l=<FH>) {
      if ($l=~/^.*n\(\".*\"\).*$/) {
	$l=~s/^(.*)n\((\".*\")\)(.*)\n$/$2/;
	$k=$l;
	$n=2;
      }

      if($n==1)
	{
	  if ($l=~/^.*\"\).*$/) {
	    $l=~s/^(.*\")\)(.*)\n$/$1/;
	    $l=~s/\n//;
	    $k=$k . "\n" . $l;
	    $n=2;

	  }
	  else
	    {
	      $l=~s/\n//;
	      $k=$k . "\n" . $l;
	    }
	}

      if($n==0)
	{
	  if ($l=~/^.*n\(\".*$/) {
	    $l=~s/^(.*)n\((\".*)\n$/$2/;
	    $k=$l;
	    $n=1;
	  }
	}

      if($n==2)
	{
	  if ($locale{$k} eq "\"\"" || !($locale{$k})) {
	    $locale{$k}="\"\"";
	  }
	  $n=0;
	}
#      if ($l=~/^.*n\(\".*\"\).*$/) {
#	$l=~s/^(.*)n\((\".*\")\)(.*)\n$/$2/;
	#print ($locale{$l} . "\n");
#	if ($locale{$l} eq "\"\"" || !($locale{$l})) {
#	  $locale{$l}="\"\"";
#	}
#      }
    }
    close(FH);
  }

while ( ($key, $value) = each %locale) {
   print OF ("loc\t". $key . "\n");
   print OF ("\t$value;\n\n");
}


print OF "\n";
print " done.\n";
close (OF);
