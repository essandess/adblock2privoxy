#!/usr/bin/perl

# Updated: 2013-06-07 11:37:25

###############################################################################
#       privoxy-bl-upd.pl
###############################################################################
#       Copyright 2010 Arcady N. Shpak https://sites.google.com/site/rpfteam/
#
#       This program is free software; you can redistribute it and/or modify
#       it under the terms of the GNU General Public License as published by
#       the Free Software Foundation; either version 2 of the License, or
#       (at your option) any later version.
#
#       This program is distributed in the hope that it will be useful,
#       but WITHOUT ANY WARRANTY; without even the implied warranty of
#       MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#       GNU General Public License for more details.
#
#       You should have received a copy of the GNU General Public License
#       along with this program; if not, write to the Free Software
#       Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
#       MA 02110-1301, USA.
################################################################################

# codepage utf8

use strict;

# URL blacklists of the Adblock
my @blacklists = (
	'https://easylist-downloads.adblockplus.org/easylist.txt',
	'https://easylist-downloads.adblockplus.org/advblock.txt',
	'https://easylist-downloads.adblockplus.org/cntblock.txt',
	'https://easylist-downloads.adblockplus.org/bitblock.txt',
	'https://easylist-downloads.adblockplus.org/antinuha.txt',
	);
# Directory with the configuration Privoxy
my $cfg = '/etc/privoxy';
# Temporary directory
my $tmp = '/tmp';
# Temporary file name (you can not change)
my $file = '/adblock.tmp';

open OUT, ">>$cfg/adblock.action.tmp" || die "Can't open data-file $cfg/adblock.action.tmp\n", $!;

# Print header
print OUT '# This blacklist automatically generated at privoxy-bl-upd script', "\n";
print OUT '# Arcady N. Shpak https://sites.google.com/site/rpfteam/', "\n";
print OUT '# Date: ', scalar localtime, "\n"x2;
print OUT '{-filter -handle-as-empty-document +handle-as-image}', "\n";
print OUT '/.*\.(bmp|gif|jpe?g|ico|png|swf)($|\?)', "\n"x2;
print OUT '{-handle-as-image}', "\n";
print OUT '/.*\.(asp|cgi|css|s?html?|js|php|pl)($|\?)', "\n"x2;

&process($_) foreach @blacklists;

close OUT;

# Changing permissions
system "chmod a+rw $cfg/adblock.action";

# Replace the old file to the new
if(-e "$cfg/adblock.action.tmp") {
	unlink "$cfg/adblock.action.bak";
	rename "$cfg/adblock.action", "$cfg/adblock.action.bak";
	rename "$cfg/adblock.action.tmp", "$cfg/adblock.action" || die "Can't rename file $cfg/adblock.action.tmp\n", $!;
	unlink "$cfg/adblock.action.bak";
}

# Remove the temporary file
unlink $tmp.$file if(-e $tmp.$file);

exit;


sub process {
	my $url = shift; $url =~ /([^\/]+)$/;
	my $name = $1;
	my (@white, @black);
	unlink $tmp.$file if(-e $tmp.$file);
	return if(system "wget -c --no-check-certificate $url -O $tmp$file");
	open IN, $tmp.$file || die "Can't open data-file $tmp.$file\n", $!;
	while(<IN>) {
		$_ = &clean($_);               # Pretreatment
		next if($_ =~ /^$/);           # Skip empty lines
		next if($_ =~ /\!/);           # Skipping comments
		next if($_ =~ /\#/);           # Skip filter styles
		next if($_ =~ /\$/);           # Skip the mask with the additions
		next if($_ =~ /[\[\(\\]/);     # Skip the regexps
		# Add a line to the white list
		if($_ =~ /^\@.*/) { push @white, &mask($_); next;  }
		# Add a line to the black list
		if($_ =~ /^(\*|\||).*/) { push @black, &mask($_); next; }
	}
	close IN;
	if($#black) {
		@black = sort @black;
		print OUT "{+block{$name}}\n", @black, "\n";
	}
	if($#white) {
		@white = sort @white;
		print OUT "{-block}\n", @white, "\n";
	}
}

sub mask {
	my($host, $path);
	# Prepare the URL for further processing
	s/\s+//g;                           # Remove all spaces in the string
	s/^[\@]+//g;                        # Remove the negative sign of the mask
	s/^\/{2,}//g;                       # Remove leading double '/'
	s/^:+//g;                           # Remove entry ':'
	s/^.*\$.*$//g;                      # Remove mask with an additional attribute block
	s/^([^\|\.]+)/\/*$1/i;
	s/^(@+|)\|{2}/$1\./g;               # Replace initial '| |' to '.*' (The coincidence of the end of the domain)
	s/^(@+|)\|{1}/$1\//g;               # Replace initial '|' a '.' (Exact domain)
	s/\|+$/\$/;                         # Replace the final '|' to '$'
	s/\^/\//g;                          # Replace '^' to '/'
	s/\|\*+//g;                         # Remove trailing '| *'
	s/^(@+|).*?:\/\/+/$1/g;             # Remove the names of the protocol (ftp, http, https)
	s/\/{2,}/\//g;                      # Remove extra '/'
	s/^.*\|.*$//g;

	s/^([^\/]*)(?:(\/.*)|)$//;
	$host = $1; $path = $2;
	$host =~ s/^www\./\./i;             # We clean domains from 'www'

	# Convert metacharacters in the corresponding regexps
	$path =~ s/\*{2,}/*/g;              # Removing redundant '*'
	$path =~ s/([\\\?\+\&\.\,])/\\\1/g; # Replace the wildcard '\? + &. '
	$path =~ s/\*/.*/g;                 # Replace the wildcard '*'

	return "$host$path\n";
}

sub clean {
    # We clean line of the total surplus
    s/^\s+|\s+$//g;
    s/[\r\n]+//g;
    return $_;
}

1;
