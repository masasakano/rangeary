# -*- coding: utf-8 -*-
require 'time'
require 'optparse'

ChangeLog = './ChangeLog'
BANNER = <<"__EOF__"
Usage: ruby #{File.basename($0)} [options]
 In dafault, everything is output to STDOUT.
 Only if --commit --no-dryrun, ./ChangeLog is overwritten and git commit -a!

 Examples:
  % ruby #{File.basename($0)} -m 'A change.' > STDOUT
  % ruby #{File.basename($0)} --commit -v 'v1.2.3' -m 'A change.' > STDOUT
  % ruby #{File.basename($0)} --no-dryrun -v 'v1.2.3' -m 'A change.' > ChangeLog
  % ruby #{File.basename($0)} --no-dryrun --commit -v 'v1.2.3' -m 'A change.'

__EOF__
# <<'__EOF__'	# for Emacs color hilit

opt = OptionParser.new(BANNER)
OPTS = {
  :dryrun => true,
  :author => 'Masa Saskano'
}

opt.on('-m MESSAGE', 'message for git commit -a -m') {|v| OPTS[:m] = v}
opt.on('-v VERSION_STRING', 'tag for git tag (eg. v1.2.3)') {|v| OPTS[:v] = v}
opt.on('--author[=AUTHOR]', "Author name for the newest message.") {|v| OPTS[:author] = v}
opt.on('--[no-]commit', "git commit if true (Default: False) after ChangeLog is automatically overwritten.") {|v| OPTS[:commmit] = v}
opt.on('--[no-]dryrun', "Dryrun (default)") {|v| OPTS[:dryrun] = v}

opt.parse!(ARGV)

if ARGV.size > 0
  warn "No argument but options only!  See --help."
  exit 1
end

## Open ChangeLog if specified so.
if ! OPTS[:dryrun] 
  if File.writable?(ChangeLog)
    ioOut = open(ChangeLog, 'w')
  else
    raise "File(#{ChangeLog}) is not writable or existent."
  end
else
  ioOut = $stdout
end

begin
  ## Writing ChangeLog 

  if OPTS[:m]
    message = OPTS[:m].chomp.gsub(/(\r?\n|\r)/m, '\1'+"\t")
    ioOut.print "-----\n"
    if OPTS[:v]
      ioOut.printf "(Version: %s)\n", OPTS[:v] 
    end
    ioOut.printf "%s  %s\n * %s\n\n", Time.now.to_date.to_s, OPTS[:author], message
  end

  author = nil
  IO.popen("git log --decorate=full", "r"){ |ioIn|
    ioIn.each_line do |line|
      if /^Date:\s*(\S.+)/ =~ line
        date = Time.parse($1).to_date.to_s
        ioOut.printf("%s  %s\n", date, author)
      elsif /^Author:\s+(\S.+) </ =~ line
        author=$1
        next
      else
        line.sub!(/^commit .*tags\/([^,)]*).*/){"-----\n(Version: #{$1})"}
        line.sub!(/^commit .*/,"-----")
        line.sub!(/^   /," *")
        ioOut.print line
      end
    end
  }

ensure
  if ioOut != $stdout
    ioOut.close
    printf "Date: %s\n", Time.now
    system "ls -lT "+ChangeLog
  end
end


# @param [String] str From the first delimiter to the end of the line.  No escape sequence.
def strWithinQ(str)
  deli1 = str[0,1]
  case deli1
  when '(',
    deli2 = ')'
  when '{'
    deli2 = '}'
  when '['
    deli2 = ']'
  else
    deli2 = deli1
  end
  deli2inreg = Regexp.quote(deli2)
  str.sub(/.([^#{deli2inreg}]+)#{deli2inreg}.*/, '\1')
end


############ Commit ##########

if OPTS[:commmit]
  
  ## Checking the version (and date) in *.gemspec
  if OPTS[:v]
    print "\n======= Checking version consistency ========\n"
    ver4gem = OPTS[:v].sub(/^\D+/,'')
    gemspecsuffix = '.gemspec' 
    Dir.glob('*'+gemspecsuffix).each do |eachf|
      hsFlag = {:isin => false, :isVersionFound => false}
      open(eachf){ |ior|
        ior.each_line do |line|
          if hsFlag[:isin]
            line.chomp!

            if /^\s*#/ =~ line
              # Do nothing

            elsif /\.date\s*=\s*(%[qQ])?(\S+)/ =~ line
              dateInGemspec = $2
              dateInGemspec = strWithinQ(dateInGemspec)
              if Time.parse(dateInGemspec).to_date == Time.now.to_date
                puts "  Date (#{dateInGemspec}) in .gemspec is today -- OK."
              else
                warn "Date (#{dateInGemspec}) in .gemspec is NOT today (#{Time.now.to_date})!"
              end

            elsif /\.version\s*=\s*(%[qQ])?(\S+)/ =~ line
#puts "DEBUG:Found version in "+eachf
              hsFlag[:isVersionFound] = true
              qDeli = $1
              verInGemspec = $2
              verInGemspec = strWithinQ(verInGemspec)
              # deli1 = verInGemspec[0]
              # if qDeli.nil? || /[({\[]/ !~ deli1
              #   verInGemspec.sub!(/(.)(.+?)\1/, '\2')
              # else
              #   case deli1
              #   when '('
              #     deli2 = ')'
              #   when '{'
              #     deli2 = '}'
              #   when '['
              #     deli2 = ']'
              #   else
              #     raise
              #   end
              #   verInGemspec.sub!(/(.)(.+?)#{Regexp.quote(deli2)}/, '\2')
              # end

              if ver4gem == verInGemspec
                project = File.basename(eachf, gemspecsuffix)
                arGem = Dir.glob(project+'-*.gem')
                arGemGrep = arGem.grep(/#{Regexp.quote(ver4gem)}/)
                if arGemGrep.size > 0
                  warn '======='
                  warn "ERROR: The GEM file #{arGemGrep.inspect} with the version (#{ver4gem}} for given (#{OPTS[:v]}) exists!"
                  warn '======='
                  exit 1
                end

                if /(\d+)/ =~ ver4gem[-1]
                  prevVer = $1.to_i
                  if prevVer > 0
                    prevver4gem = ver4gem.sub(/\.(\d+)$/, '.'+(prevVer-1).to_s)
                    arGemGrep = arGem.grep(/#{Regexp.quote(prevver4gem)}/)
                    if arGemGrep.size > 0
                      puts "For (#{eachf}) --- OK!"
                      puts "  Version (#{verInGemspec}) in (#{eachf}) is consistent with (#{OPTS[:v]})."
                      puts "  No *.gem file is found for this version."
                      puts "  Gem of previous version is found #{arGemGrep.inspect}."
                    else
                      puts "For (#{eachf}) --- Please check!"
                      puts "  Version (#{verInGemspec}) in (#{eachf}) is consistent with (#{OPTS[:v]})."
                      puts "  No *.gem file is found for this version."
                      puts "  But no *.gem file for the previous version (#{prevver4gem}) is found in (#{project+'-*.gem'}), either."
                      puts "  Please check!!!!!"
                    end
                  else
                      puts "For (#{eachf}) --- OK, probably!"
                      puts "  Version (#{verInGemspec}) in (#{eachf}) is consistent with (#{OPTS[:v]})."
                      puts "  No *.gem file is found for this version."
                      puts "  Note: Please check the minor and major versions."
                      print "   Existing files: "; p arGem 
                  end
                else
                      puts "For (#{eachf}) --- OK, probably!"
                      puts "  Version (#{verInGemspec}) in (#{eachf}) is consistent with (#{OPTS[:v]})."
                      puts "  No *.gem file is found for this version."
                      puts "  Note: Please check the version consistency with the previous ones, as I do not know..."
                      print "   Existing files: "; p arGem 
                end

              else
                warn '======='
                warn "ERROR: Version (#{verInGemspec}) in (#{eachf}) is inconsistent with given (#{OPTS[:v]})!"
                warn '======='
                exit 1
              end
            else
              
#puts "DEBUG:fail (#{line.chomp})..."
            end	# if /^\s*#/ =~ line
              
          elsif /^\s*#{Regexp.quote("Gem::Specification.new")}/ =~ line
            hsFlag[:isin] = true
#puts "DEBUG:Found specification in "+eachf

          end	# if hsFlag[:isin]
        end	# ior.each_line do |line|
      }		# open(eachf){ |ior|

      if ! hsFlag[:isVersionFound]
        warn '======='
        warn "WARNING: Failed to find the version line in (#{eachf})...  Please check!"
        warn '======='
      end

    end		# Dir.glob("*.gemspec").each do |eachf|
  end		# if OPTS[:v]

  ##### End of Version checking...

  if ! OPTS[:m]
    warn "Please specify the message...  Abort."
    return
  end

  print "\n======= git starts ========\n"
  if OPTS[:dryrun] 
    puts 'Dryrun......  (Add --no-dryrun for the real run, without "| less".)'
  else
    puts 'Running......'
  end

  com1 = "git commit -a -m '" + message + "'"
  com2 = 'git tag ' + (OPTS[:v] || '')

  puts com1
  if ! OPTS[:dryrun] 
    ret1 = system(com1)
    if ret1
      # Do nothing
    else
      warn "ERROR: git failed!"
      if OPTS[:v]
        warn "     : Command(#{com2}) is not run, accordingly!"
      end
    end
  end

  ret2 = true
  if OPTS[:v] && (ret1 || OPTS[:dryrun])
    puts com2
    if ! OPTS[:dryrun] 
      ret2 = system(com2)
      if ret2
        # Do nothing
      else
        warn "ERROR: git tag failed!"
      end
    end
  end

  print <<'__EOF__'

Please check the status with
 % git log --decorate=full | less
If you find any problem, you can undo the commit with
 % git reset --soft 'HEAD^'
__EOF__

  if OPTS[:v] && ret2
    print <<"__EOF__"
 % git tag -d #{OPTS[:v]}
__EOF__
  end
end	# if OPTS[:commmit]
