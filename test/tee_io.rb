
# Taken from <https://gist.github.com/masasakano/789030a7dc5313bd343b2de967a93200>

require "tempfile"

# TeeIO to "tee" (or splice) an IO
#
# Basically this overwrites IO#write .
#
# Note this does not capture IO#syswrite
#
# The main part but {TeeIO.suppress_io} is taken from
# {https://stackoverflow.com/a/9439298/3577922}
#
# Main use of this class is {TeeIO.suppress_io}
#
# @author Masa Sakano
#
class TeeIO < IO
  def initialize io_orig, io_file
    @orig = io_orig
    @file = io_file
  end
  def write string
    @file.write string
    @orig.write string
  end

  # IO outputs are suppressed in the given block with a copy/alternative provided.
  #
  # If +ENV['DISPLAY_STDERR']+ (or the name provided) is set,
  # the original is output as normal, while it is redirected to IO, too.
  #
  # This is especially designed for testing framework.  In testing,
  # you may deliberately test lots of situations where messages
  # are printed to the terminal.  You may want to suppress them,
  # providing the messages are what you expect.  But it is laborious
  # to write test code to check all the messages, and so you may just
  # want to suppress all.  However, from time to time you may want to
  # switch to see the messages to make sure nothing unexpected is happening.
  #
  # This method provides the flexibility. You can suppress the message,
  # while you can still access them if you want, and you can switch off
  # suppressing by providing an environmental variable.
  #
  # @note For the given IO, IO#sync=true should be set beforehand.
  #
  # @example suppressing STDOUT
  #   puts "1: This is displayed."
  #   suppress_io($stdout, envanme: "DISPLAY_STDOUT"){ |iorw|
  #     puts "2: This is suppressed."
  #     iorw.rewind
  #     $stderr.print "3: Message read from IO: "
  #     $stderr.puts iorw.read  # => "2: This is suppressed."
  #   }
  #   puts "4: This is displayed."
  #
  # @see https://gist.github.com/moertel/11091573
  #
  # @param ioin [IO] IO to suppress
  # @param envname [String] Name of the Environmental variable
  # @yield [IO] the given IO is redirected (or copied)
  def self.suppress_io(ioin=$stderr, envname: 'DISPLAY_STDERR')
    display_stderr = (ENV[envname] && !ENV[envname].empty? && ENV[envname] != "0")
    iorw = Tempfile.new(File.basename(__FILE__))
    iorw.sync = true
    original_stderr = $stderr.clone
  
    tee = TeeIO.new $stderr, iorw
    if display_stderr
      $stderr = tee
    else
      #$stderr.reopen File.new('/dev/null', 'w')
      $stderr.reopen iorw
    end
    yield(iorw)
  ensure
    if display_stderr
      $stderr = original_stderr
    else
      $stderr.reopen original_stderr
    end
    iorw.close if iorw
  end
end


############### Demonstrations ###############

if $0 == __FILE__
  $stdout.sync = true
  $stderr.sync = true  # recommended!
  $stderr.puts "1: Before suppression."
  TeeIO.suppress_io{ |iorw|
    $stderr.puts "2: This is suppressed."
    print "3: Message read from IO: "
    iorw.rewind
    puts iorw.read
  }
  $stderr.puts "4: After first suppression."

  ENV['DISPLAY_STDERR'] = "1"
  TeeIO.suppress_io{ |iorw|
    $stderr.puts "5: This is not suppressed."
    print "6: Message read from IO: "
    iorw.rewind
    puts iorw.read
  }
  $stderr.puts "7: After second suppression."
  $stderr.puts "Messages 1-7 except 2 should be printed (the order may vary)."
end
