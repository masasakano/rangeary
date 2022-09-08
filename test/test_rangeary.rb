# -*- encoding: utf-8 -*-

require 'tempfile'
require_relative "tee_io.rb"  # Library to suppress STDERR/STDOUT <https://gist.github.com/masasakano/789030a7dc5313bd343b2de967a93200>

$stdout.sync=true
$stderr.sync=true
print "$LOAD_PATH=#{$LOAD_PATH}" if $DEBUG
arlibbase = %w(rangeary)

arlibrelbase = arlibbase.map{|i| "../lib/"+i}

arlibrelbase.each do |elibbase|
  require_relative elibbase
end	# arlibbase.each do |elibbase|

print "NOTE: Library relative paths: "; p arlibrelbase
arlibbase4full = arlibbase.map{|i| i.sub(%r@^(../)+@, "")}+%w(range_extd)
puts  "NOTE: Library full paths for #{arlibbase4full.inspect}: "
arlibbase4full.each do |elibbase|
  ar = $LOADED_FEATURES.grep(/(^|\/)#{Regexp.quote(File.basename(elibbase))}(\.rb)?$/).uniq
  print elibbase+": " if ar.empty?; p ar
end

#################################################
# Unit Test
#################################################

#if $0 == __FILE__
  # require 'minitest/unit'
  require 'minitest/autorun'
  # MiniTest::Unit.autorun

  # Taken from ((<URL:http://www.ruby-doc.org/core-2.1.1/Range.html>))
  class Xs                # represent a string of 'x's
    include Comparable
    attr :length
    def initialize(n)
      @length = n
    end
    def succ
      Xs.new(@length + 1)
    end
    def <=>(other)
      @length <=> other.length
    end
    def to_s
      sprintf "%2d #{inspect}", @length
    end
    def inspect
      'x' * @length
    end
  end

  # Used in test_overwrite_compare
  class CLComparable
    include Comparable
    def <=>(c)
      # Badly designed, deliberately.
      if c == 7
        "XXX"
      elsif c == 8
        __method__	# => :<=>
      else
        nil
        # super		# BTW, this is the statement that should be.
      end
    end
  end

  # Used in test_rangeextd_new_infinity
  # The absolute minimum Comparable class.
  class CLC2
    include Comparable
  end

  # Used in test_range_c3c4 and test_rangeextd_new_infinity_c3
  class CLC3
    include Comparable
    # alias :original_compare :<=> if !self.method_defined?(:original_compare)	# No overwriting.
    def <=>(c)
      if c == 7
        "XXX"	# Bad statement.  Just for the sake of test.
      elsif c == 8
        -1
      elsif c.class == CLC4
        -1	# basically, CLC3 < CLC4
      else       # When self does not know what to do with c.
        super    # to call Object#<=>
        #original_compare(c)  # to call the original
      end
    end
  end

  # Used in test_range_c3c4
  class CLC4
    include Comparable
    def <=>(c)
      if c.class == CLC3
        1	# basically, CLC3 < CLC4
      else
        super
      end
    end
  end

  # Used in test_bsearch_special
  class Special
    def [](f)
      (f>3.5 && f<4) ? true : false
    end
  end

  def RaE(*rest)
    RangeExtd(*rest)
  end

  class TestUnitFoo < MiniTest::Test
    T = true
    F = false
    RaN = Rangeary(RangeExtd::NONE)
    RaA = Rangeary(RangeExtd::ALL)
    InfF = Float::INFINITY
    InfP = RangeExtd::Infinity::POSITIVE
    InfN = RangeExtd::Infinity::NEGATIVE

    def setup
      @ib = 1
      @ie = 6
      @r11 = (@ib..@ie)		# incl, incl
      @r12 = (@ib...@ie)	# incl, excl
      @s11 = RangeExtd.new(@r11,  false)	# (1..6)   incl, incl
      @s21 = RangeExtd.new(@r11,  true)	# (1<..6)  excl, incl
      @s12 = RangeExtd.new(@r12,  false)	# (1...6)  incl, excl
      @s22 = RangeExtd.new(@r12,  true)	# (1<...6) excl, excl
    end
    # teardown is not often used.
    def teardown
      @foo = nil
    end

    # User method
    def conjRE(r1, r2)
      Rangeary.class_eval{ conjunctionRangeExtd(r1, r2) }
    end


    def test__get_infinities
      rn = Rangeary.new
      hs = rn.instance_eval{_build_infinities({}, [(77..78)])}
      assert_equal(-InfF, hs[:negative])
      assert_equal( InfF, hs[:positive])
      hs = rn.instance_eval{_build_infinities({}, [(77...InfF)])}
      assert_equal(-InfF, hs[:negative])
      assert_equal( InfF, hs[:positive])
      hs = rn.instance_eval{_build_infinities({}, [(-InfF..78)])}
      assert_equal(-InfF, hs[:negative])
      assert_equal( InfF, hs[:positive])
      #hs = rn.instance_eval{_get_infinities [(77...InfF)], guess_strict: true}
      #assert_equal false, hs[:negative]
      #assert_equal( InfF, hs[:positive])
    end

    def test_comparable_end
      assert_equal 3,    Rangeary.comparable_end(0...3)
      assert_equal InfF, Rangeary.comparable_end(0...Float::INFINITY)
      assert_nil         Rangeary.comparable_end(RangeExtd::NONE)
      _ = nil..nil  rescue return  # nil..nil may raise Exception in some Ruby versions
      #assert_nil         Rangeary.comparable_end(nil..nil)  # Ruby-2.6
      assert_equal InfP, Rangeary.comparable_end(nil..nil)
      assert_equal InfP, Rangeary.comparable_end(nil...nil)
    end

    # Since Ruby-2.7
    def test_comparable_beginend
      assert_raises(RangeError){Rangeary.comparable_beginend(RangeExtd::Nowhere::NOWHERE..RangeExtd::Nowhere::NOWHERE) }	# ! Range#valid?
      assert_raises(RangeError){Rangeary.comparable_beginend(?a...?a) }	# because !Range#valid?
      assert_equal RaE(5...9),        Rangeary.comparable_beginend(5...9)
      assert_equal RaE(?a..?b, true), Rangeary.comparable_beginend(RaE(?a, ?b, true))
      assert_equal RaE(?a..?b, true), Rangeary.comparable_beginend(RaE(?a, ?b, true))
      assert_equal RaE(InfN...InfP),  Rangeary.comparable_beginend(...nil)
      assert_equal RaE(-InfF...9.3, T), Rangeary.comparable_beginend(RaE(...9.3, true))
      assert_equal RaE(9.3..InfF),  Rangeary.comparable_beginend(9.3..)
      assert_equal RaE(?a...InfP),  Rangeary.comparable_beginend(?a...)

      # infinities are given.
      hsinf = {negative: ?a, positive: ?z}
      assert_equal RangeExtd(?b...?z),   Rangeary.comparable_beginend(?b..., infinities: hsinf)
      assert_equal RangeExtd(?a..?d, T), Rangeary.comparable_beginend(RaE(..?d, T), infinities: hsinf)
      ran = RaE(?c.., T)
      hsinf = {positive: InfP}
      assert_equal RaE(?c..InfP, T),     Rangeary.comparable_beginend(ran, infinities: hsinf)
      assert_equal RaE(?c..InfP, T),     Rangeary.comparable_beginend(ran, infinities: {positive: nil}), "nil in infinities should be ignored in Rangeary.comparable_beginend()"

      # instance-method version
      hsinf = {negative: ?a, positive: ?z}
      ran = RaE(..?d, T)
      assert_raises(ArgumentError){ Rangeary(ran, **hsinf) }  # raise: specified/inherited negative infinity ("a") is not small enough or inconsistent: (<=> nil)
      ran = RaE(?a..?d, T)
      rangy = Rangeary(ran, **hsinf)
      assert_equal RaE(?a..?d, T), rangy.send(:_comparable_beginend, ran)

      ran = RaE(?c.., T)
      assert_raises(ArgumentError){ Rangeary(ran, **hsinf) }  # raise: specified/inherited negative infinity ("z") is not large enough or inconsistent: (<=> nil)
      rangy = Rangeary(ran)
      assert_equal RaE(?c..InfP, T), rangy.send(:_comparable_beginend, ran)

      ran = RaE(?c..?z, T)
      rangy = Rangeary(ran, **hsinf)
      assert_equal ran,              rangy.send(:_comparable_beginend, ran)
    end

    def test_sort_ranges
      assert_equal [RangeExtd::NONE, 2..5, RangeExtd(2..5,9), 3..5, 3...6, 3..6], Rangeary.sort_ranges(RangeExtd(2..5,9), 2..5,3..6,3...6,3..5,RangeExtd::NONE)

      inf = Float::INFINITY
      ra1 = RangeExtd(5,  inf, false, false)  # (5..inf)
      ra2 = RangeExtd(5,  inf, false, true)   # (5...inf)
      assert_equal [(5..9), ra2, ra1],   Rangeary.sort_ranges(ra1, ra2,  (5..9))

      inf = RangeExtd::Infinity::POSITIVE
      ra1 = RangeExtd(?a, inf, false, false)  # (?a..inf)
      ra2 = RangeExtd(?a, inf, false, true)   # (?a...inf)
      assert_equal [(?a..?d), ra2, ra1], Rangeary.sort_ranges(ra1, ra2, (?a..?d))

      # Ruby 2.6 Endless Range
      assert_equal [(5..9),   (5...nil),  (5..nil)],  Rangeary.sort_ranges((5..nil),  (5...nil),  (5..9))
      assert_equal [(?a..?d), (?a...nil), (?a..nil)], Rangeary.sort_ranges((?a..nil), (?a...nil), (?a..?d))

      # Ruby 2.7 Beginless Range
      ran = RangeExtd(nil...5, T)
      assert_equal [(nil..5),  ran, (1..5)  ], Rangeary.sort_ranges((1..5),   ran,  (..5))
      ran = RangeExtd(nil...?d, T)
      assert_equal [(nil..?d), ran, (?b..?d)], Rangeary.sort_ranges(ran, (?b..?d), (..?d))
      assert_equal [(nil..?d), ran, (?b..?d)], Rangeary.sort_ranges(ran, (?b..?d), (..?d))
      ran = RangeExtd(..9, T)
      assert_equal RangeExtd::NONE,  Rangeary.sort_ranges(RaA, ran, RaN)[0]
    end	# def test_sort_ranges

    def test_new
      assert_equal [2..5, 8..8], Rangeary.new(RangeExtd(8..8), 2..5).to_a
      assert_equal [2..5, 8..8], Rangeary(RangeExtd(8..8), 2..5).to_a
      rs = Rangeary.new([3..5, -6..-1, -4..-3, 2..4, 8..8])
      assert_equal [-6..-1, 2..5, 8..8], rs.to_a

      assert_raises(NoMethodError){ Rangeary.new(3..5).reverse }	# => undefined method `reverse' for <Rangeary:[3..5]>:Rangeary
      assert_raises(RuntimeError){  Rangeary.new(3..5).reverse! }	# => can't modify frozen Rangeary
      #assert_raises(ArgumentError){ Rangeary.new(3..5, nil..nil) }	# => invalid parameter for RangeExtd, hence for Rangeary (nil..nil).
      assert_equal([..nil], Rangeary.new(3..5, RangeExtd::NONE, RangeExtd::NONE, nil..nil).to_a)	# nil..nil used to be an invalid parameter for RangeExtd, hence for Rangeary, too, before Ruby-2.7 (or 2.6?).
      ran  = [..6, 9..InfF]
      rang = Rangeary(*ran)
      hsinf = {negative: nil, positive: InfF}
      assert_equal ran, rang
      assert_equal hsinf, rang.infinities  # Hash == HashInf

      rang2 = Rangeary(11..13, Rangeary(*rang), 15..16)
      assert_equal ran, rang2
      assert_equal hsinf, rang2.infinities

      assert_raises(ArgumentError, "this should fail because whereas the guessed positive infinity from the first one is Float:INFINITY, the second given infinity is +RangeExtd::Infinity::POSITIVE+, which is not comparable with +Float:INFINITY+ (although it is comparable with general Float)."
                   ){ Rangeary(..6, 9..InfP) }	# => "comparison of Float with RangeExtd::Infinity failed"
      assert_raises(ArgumentError){ Rangeary.new(3..5, 3..1) }	# => invalid parameter for RangeExtd
      assert_raises(ArgumentError){ Rangeary.new(3..5, 4...4) }	# => invalid parameter for RangeExtd
      assert_raises(ArgumentError){ Rangeary(?d..?f, negative: ?a, positive: ?b) }	# => negative-infinity is larger than ?d
      assert_raises(ArgumentError){ Rangeary(?d..?f, negative: ?e, positive: ?z) }	# => negative-infinity is larger than ?d
      assert_raises(ArgumentError){   Rangeary(?b..?d, ?f...?h, ?g...?j, negative: ?b, positive: ?i) }
      assert_equal [?b..?d, ?f...?j], Rangeary(?b..?d, ?f...?h, ?g...?j, negative: ?b, positive: ?j).to_a
      assert_raises(ArgumentError){ Rangeary(RangeExtd::NONE, negative: ?c, positive: ?a) }  # negative is smaller than positive infinities
      assert_raises(ArgumentError){ Rangeary(?a..?d, negative: -InfF) }  # contradictory infinities.
    end	# def test_new

    def test_new_infinity
      #assert_equal [RangeExtd::NONE], Rangeary.new(RangeExtd::NONE).to_a
      assert_equal [-Float::INFINITY..4],  Rangeary(-Float::INFINITY..4).to_a
      assert_equal [-Float::INFINITY...4], Rangeary(-Float::INFINITY...4).to_a
      r1 = RangeExtd(-Float::INFINITY...6)
      r2 = RangeExtd(2,Float::INFINITY,9)
      r3 = RangeExtd(18,Float::INFINITY,9)
      # assert_equal [RangeExtd::ALL], Rangeary(r1,r2).to_a  # Before Ver.1
      assert_equal [-InfF..InfF], Rangeary(r1,r2).to_a
      assert ! Rangeary(r1,r2).to_a[0].is_all?
      assert_equal [-Float::INFINITY...8, RangeExtd(8..10,9), r3], Rangeary(r1,5...8,RangeExtd(8..9,9),9..10,r3).to_a

      r4 = RangeExtd(InfN...?h)
      r5 = RangeExtd(?c, InfP, T)
      assert_equal [RangeExtd::ALL], Rangeary(r4,r5).to_a

      r6 = Rangeary(InfF..InfF)
      assert_equal [RangeExtd::NONE], r6  # Strange zero-sized one, handled by _replace_inf_inf(arin)  # Note that irb displays [RangeExtd::NONE] as [nil...nil]
      assert  r6.infinities.status_is_nil?(:positive), "confidence of the infinities of the meaningless Range should be nil: infinities=#{r6.infinities.inspect}"
      assert  r6.infinities.status_is_nil?(:negative)
      assert_equal false, r6.infinities[:positive]
      assert_equal false, r6.infinities[:negative]

      assert_equal [r1], Rangeary(r1,(-InfF..-InfF))
      assert_equal [r1], Rangeary(r1,( InfF..InfF))

      r11 = Rangeary(?f..)
      err = assert_raises(ArgumentError){ Rangeary(r11,  positive: ?t) }
      assert_match(/positive .+ not large enough or inconsistent\b/, err.message) # (_validate_infinities) => specified positive infinities ("t") are not small enough or inconsistent: (<=> nil)
      assert_raises(ArgumentError){ Rangeary(?f.., positive: ?t) }
      assert_raises(ArgumentError){ Rangeary(..?f, negative: ?a) }
      assert_raises(ArgumentError){ Rangeary(3..,       positive: 99) }
      assert_raises(ArgumentError){ Rangeary(3..InfF,   positive: 99) }
      assert_raises(ArgumentError){ Rangeary(-InfF..99, negative: -6) }
      r12 = Rangeary(-InfF..99, negative: InfN, positive: InfP)
      assert_equal InfP, r12.infinities[:positive]
      assert             r12.infinities.definite? :negative

      ## The following used to be the case?
      #err = assert_raises(ArgumentError){ Rangeary(r1,(InfF..InfF)) }
      #assert_match(/not small enough or inconsistent\b/, err.message) # (_validate_infinities) => specified negative infinities (Infinity) are not small enough or inconsistent.
    end	# def test_new_infinity

    def test_new_infinity2
      ## doc inside the code (Rangeary#initialize)
      ran2 = ("f".."k")
      rae1 = RaE("k"..nil, true)
      r3 = ~(Rangeary(ran2, negative: "d"))
      assert_equal Rangeary("d"..."f", rae1), r3
      assert_equal "d", r3.infinities[:negative]
      r4 =   Rangeary(r3, negative: "a")
      assert_equal "b", Rangeary(r4, negative: "b").infinities[:negative]
      r5 =  ~Rangeary(r4)
      assert_equal Rangeary("a"..."d", "f".."k"), r5

      r6 = r3 + Rangeary("c".."d", negative: "a")
      assert_equal Rangeary("c"..."f", rae1), r6

      assert_equal r3, r3*(..nil), "conjunction with (..nil) should never raise an Exception, because nil has a lower priority than other infinities."

      r7 = nil
      ################# puts "\nNOTE: A warning is suppressed during this testing. To display it, set DISPLAY_STDERR=1" if !ENV['DISPLAY_STDERR'] || ENV['DISPLAY_STDERR'].empty?   ### This statement is removed because there are no warnings displayed in the end!
     #TeeIO.suppress_io{|iorw|
      r7 = r3*Rangeary("c".."d", negative: "a")
      #iorw.rewind
      #assert_match(/Inconsistent negative infinities are found\b/, iorw.read)  # util.rb: _validate_select_infinities()
     #}
      assert_equal Rangeary("d".."d"), r7
      assert_equal "a", r7.infinities[:negative]
      r8 = (r3 ^ (?e..?h))
      assert_equal Rangeary("d"..."e", "f".."h", rae1), r8
    end # def test_new_infinity2

    def test_user_infinity
      # @infinities are inherited but maybe partially overwritten in initialization
      ray1 = Rangeary(?h...?m, negative: ?a, positive: ?z)
      assert_equal [?a...?h, ?m..?z], ~ray1
      assert_equal [?b...?h, ?m..?z], ~Rangeary(ray1, negative: ?b)
      assert_equal [?a...?h, ?m..?y], ~Rangeary(ray1, positive: ?y)
      assert_equal [?b...?h, ?m..?y], ~Rangeary(ray1, negative: ?b, positive: ?y)
      ray3 = ray1 + Rangeary(?i..?j, positive: "k")
      assert_equal [?h...?m], ray3
      assert_equal({positive: ?z, negative: ?a}, ray3.infinities)  # Hash-extended-with-HashInf
      assert_equal({positive: ?z, negative: ?a}, ray3.infinities(convert: false))  # Hash == Hash
      assert  ray3.infinities.definite?(:positive)
      assert  ray3.infinities.definite?(:negative)
      ray4 = Rangeary(ray3, positive: ?m)
      assert_equal ?m, ray4.infinities[:positive]
      assert           ray4.infinities.definite?(:positive)
      err = assert_raises(ArgumentError){ ray4 + [?m..?t] } # specified/inherited positive infinity ("k") is not large enough or inconsistent: (<=> "m")
      assert_match(%r@specified/inherited positive infinit.+"m".+\bnot large enough\b.+"t"@, err.message) # specified/inherited positive infinity ("m") is not large enough or inconsistent: (<=> "t")
    end

    def test_custom_infinity1
      infend = "w"
      def infend.infinite?
        true
      end
      assert  infend.infinite?

      rang = Rangeary(?d..infend)
      hsinf = {negative: nil, positive: "w"}
      assert_equal hsinf, rang.infinities  # Hash == HashInf
      assert_equal [...?d], ~rang
      rang = Rangeary(Rangeary(?a...?b), ?b...?c, ?d..infend, ?f..?j)
      assert_equal hsinf, rang.infinities
      assert_equal [?a...?c, ?d..infend], rang

      # If (?f..?z) is given, which contradicts the positive infinity "w",
      #   what will happen?
      err = assert_raises(ArgumentError, "specifying a positive infinity, InfP, as a negative infinity should raises an Exception."){
        Rangeary(?b...?c, ?d..infend, Rangeary(?f..?z, negative: InfP)) } # 'specified/inherited negative infinity (INFINITY) is not small enough or inconsistent: (<=> "f")'; b/c 
      assert_match(%r@negative infinity\b.+ is not small enough@, err.message)
      r3 = Rangeary(?b...?c, ?d..infend)
      assert_equal hsinf,  r3.infinities
      assert               r3.infinities.definite?(:positive)
      r4 = Rangeary(?b...?c, ?d..infend, Rangeary(?f..?z, positive: InfP))
      hsinf4 = hsinf.merge({positive: InfP})
      assert_equal hsinf4, r4.infinities
      assert               r4.infinities.definite?(:positive)
    end

    def test_custom_infinity2
      infend = "w"
      def infend.infinite?
        true
      end
      hsinf = {negative: nil, positive: "w"}

      ra1 = Rangeary(?b...?c, ?d..infend)
      ra2 = Rangeary(ra1, positive: InfP)
      assert_equal hsinf.merge({positive: InfP}), ra2.infinities  # Hash == HashInf
      assert_equal [?b..?z],  Rangeary(ra2, ?c..?z)

      rang = Rangeary(?b...?c, ?d..infend, Rangeary(?f..?w, negative: InfN))
      assert_equal hsinf.merge({negative: InfN}), rang.infinities
      assert_equal [?b...?c, ?d..infend], rang
    end

    def test_endless_range
      begin
        _ = 7..nil
      rescue
        return # before Ruby 2.6
      end
      assert_equal [2..Float::INFINITY], Rangeary(2..8, 3..Float::INFINITY).to_a
      assert_equal [2..nil],  Rangeary(2..8, RangeExtd(3..nil)).to_a
      assert_equal [2..nil],  Rangeary(2..8, 3..nil).to_a
      assert_equal [2...nil], Rangeary(8...nil, 2...4, 4..9).to_a
      assert_equal [2...nil], Rangeary(8...nil, 2...4, 4..9)
    end

    def test_beginless_range
      # This would raise Syntax Error in earlier versions (2.6 or earlier?) of Ruby
      assert_equal [(-Float::INFINITY)...9], Rangeary((-Float::INFINITY)..7, 3...9).to_a
      assert_equal [nil..8],  Rangeary(2..8, RaE(nil..3)).to_a
      assert_equal [nil..8],  Rangeary(2..8, nil..3).to_a
      assert_equal [nil...9], Rangeary(nil...8, ...9, 4...9).to_a
      assert_equal [nil..9],  Rangeary(nil...8, ...9, 4..9).to_a
      assert_equal [nil...9], Rangeary(nil...8, 2...4, 4...9).to_a
      assert_equal [nil...9], Rangeary(         2...4, 4...9, nil...8)
      assert_equal [nil...8], Rangeary(nil...8, 2...4, nil..7)
      assert_equal [nil...],                   Rangeary(    8..., nil...8)
      assert_equal [nil...8,    10...],        Rangeary(   10..., nil...8)
      assert_equal [nil...8, RaE(8.., true)],  Rangeary(RaE(8.., true), nil...8)

      ran = Rangeary(...8, RaE(8.., true))
      assert_raises(RangeError ){                          ran.last.last }
      assert_raises(RangeError, "for RAN=#{ran.inspect}"){ ran.last_element }
      assert_nil                 ran.last.end
      assert_raises(RangeError, "for RAN=#{ran.inspect}"){ ran.last_element }
    end

    def test_new_none	# Essentially, the tests of compact()
      ra = Rangeary(RangeExtd::NONE, RangeExtd::NONE, RangeExtd::NONE)
      assert_equal 1, ra.size
      assert_equal RangeExtd::NONE, ra[0]
      assert  ra[0].is_none?
      re = RangeExtd(0,0,9,9)
      ra = Rangeary(RangeExtd::NONE, re)
      assert_equal 1, ra.size
      assert_equal re, ra[0]
      rs = RangeExtd(?a,?a,9,9)
      ra = Rangeary(RangeExtd::NONE, rs)
      assert_equal [rs], ra.to_a
      ra = Rangeary(RangeExtd::NONE, re, 1..8)
      assert_equal [1..8], ra	# Comparable with an Array of Range(Extd)
    end

    def test_begin_end
      ra = Rangeary(-6..-1, 2..5, 8..8)
      assert_equal(-6, ra.begin)
      assert_equal  8, ra.end
      ra = Rangeary(RangeExtd(-6..-1,9), 2..5, 7...8)
      assert_equal(-6, ra.begin_element)
      assert_equal  8, ra.end_element
      ra = Rangeary(-6.3..-1, 2..5, 8..8)
      assert_equal(-6.3, ra.begin)
      ra = Rangeary(RangeExtd::NONE)
      assert_nil  ra.begin
      assert_nil  ra.end
      ra = Rangeary(RangeExtd::ALL)
      assert_equal RangeExtd::Infinity::NEGATIVE, ra.begin
      assert_equal RangeExtd::Infinity::POSITIVE, ra.end
    end

    def test_first_last_element
      # First/Last element
      rs = Rangeary.new(-6..-5, 2..5, 8..8)
      assert_equal(-6, rs.first_element)
      assert_equal([-6,-5, 2], rs.first_element(3))
      assert_equal [ 4, 5, 8], rs.last_element(3)
    end	# def test_first_last_element


    def test_disjunction
      # Plus
      rs = Rangeary.new([3..5, -6..-1, -4..-3, 2..4, 8..8])
      assert_equal rs, rs+rs
      assert_equal [-6..-1, 2..5, 8..8], rs.to_a
      assert_equal rs.to_a,              (rs+rs).to_a
      assert_equal rs.to_a,              (rs+RaN).to_a
      assert_equal rs.to_a,              (RaN+rs).to_a
      assert_equal [-6..-1, 2..5, 8..9], (rs+(8..9)).to_a
      assert (rs != (rs+(8..9)))
      assert_equal RaN, RaN + RaN
      assert_equal RaA, RaA + RaA
      assert_equal RaA, RaA + RaN
      assert_equal RaA, RaN + RaA

      # Endless Range since Ruby 2.6
      _ = 7..nil rescue return # before Ruby 2.6
      assert_equal [-6..-1, 2..5, 8...nil], rs+(8...nil)
      assert_equal [-6..-1, 2..5, 8..nil],  Rangeary(8..nil)+rs

      # Begindless Range since Ruby 2.7
      assert_equal [..-1, 2..5, 8..8],  rs+(...(-6))
      assert_equal [..-1, 2..5, 8..8],  Rangeary(..(-6))+rs
    end	# def test_disjunction

    def test_minus
      t = true
      # Minus
      rs = Rangeary.new(-6..-1, 2..5, 8..9)
      assert_equal rs, rs-( 89..95)
      assert_equal  [-6..-1, 2...4, 8..9], (rs-(4..6)).to_a
      #assert_equal [-6..-1, 2..3, 8..9], (rs-(4..6)).to_a	# <= Rangeary.subtractRange()
      assert_equal  [-6...-2,RangeExtd(2..5,t), 8..9], (rs-(-2..2)).to_a
      #assert_equal [-6..-3, 3..5, 8..9], (rs-(-2..2)).to_a	# <= Rangeary.subtractRange()
      assert_equal  [RangeExtd(-5..-1,t), 2..5, 8..9], (rs-(-9..-5)).to_a
      #assert_equal [-4..-1, 2..5, 8..9], (rs-(-9..-5)).to_a
      assert_equal [2..5, 8..9],         (rs-(-9..0)).to_a

      # assert_equal Rangeary(-Float::INFINITY...-9, RangeExtd(3..Float::INFINITY,t)), Rangeary(-9..3).negation
      # assert_equal Rangeary(RangeExtd(3..5,t), 8..9), Rangeary.conjunction(Rangeary(-6..-1, 2..5, 8..9), Rangeary(-9..3).negation)	# This fails in  def self.conjunction_orig(r1, r2)

      assert_equal  [RangeExtd(3..5,t), 8..9], (rs-(-9..3)).to_a
      #assert_equal [4..5, 8..9],         (rs-(-9..3)).to_a
      assert_equal  [-6..-1, 2..5, 8...9], (rs-(9..11)).to_a
      #assert_equal [-6..-1, 2..5, 8..8], (rs-(9..11)).to_a
      assert_equal [-6..-1, 2..5], (rs-(8..11)).to_a
      assert_equal [-6..-1, 2..5], (rs-(7..11)).to_a
      assert_equal [-6..-1, 2...5], (rs-(5..11)).to_a
      #assert_equal [-6..-1, 2..4], (rs-(5..11)).to_a
      assert_equal [-6..-1, 2...4], (rs-(4..11)).to_a
      #assert_equal [-6..-1, 2..3], (rs-(4..11)).to_a
      assert_equal rs, rs-(-99..-95)
      assert_equal rs, rs-RaN
      assert_equal RaN, RaN-rs
      assert_equal RaN, RaN-RaN
      assert_equal RaN, RaA-RaA

      # Eendless Range since Ruby 2.6
      _ = 7..nil rescue return # before Ruby 2.6
      assert_equal [-6..-1, 2...4], rs-(4...nil)
      rr = Rangeary(6..nil)
      assert       rr.infinities.definite?(:positive)
      assert_nil   rr.infinities[:positive]
      r2 = rr-(6...8)
      assert_nil   r2.infinities[:positive], "positive should be nil with :definite: "+r2.infinities.inspect
      assert_nil   r2.infinities[:positive]
      assert_equal [8..nil],  rr-(6...8)
      assert_equal [6...7],   rr-(7...nil)
      rr = Rangeary(6..9, 8..nil)
      assert_equal [8..nil],  rr-(6...8)

      # Begindless Range since Ruby 2.7
      assert_equal [3..5, 8..9], rs-(...3)
    end	# def test_minus

    # Array#equal overwritten.
    def test_equal_array
      rany = Rangeary(4..nil)
      assert_equal rany, [4..nil]
      assert_equal [4..nil], rany
      refute_equal rany, (4..nil)

      rany2 = Rangeary(1..2, 4..nil)
      refute_equal rany2, Rangeary(0..2, 4..nil)
      refute_equal rany2, [1..2, 9]
      refute_equal rany2, Rangeary(1..2, 4..8)

      assert_equal Rangeary(RangeExtd::NONE), []
      assert_equal [], Rangeary(RangeExtd::NONE)
      assert_equal Rangeary(RangeExtd::NONE), [RangeExtd::NONE]
      assert_equal [RangeExtd::NONE], Rangeary(RangeExtd::NONE)
      refute_equal rany, []
      refute_equal [], rany

      ## The following used to be all true up to Rangeary Ver.1 before Ver.2
      refute_equal rany, [4..InfF]
      refute_equal rany, [4...InfF]
      refute_equal [4...InfF], rany
      refute_equal rany2, Rangeary(1..2, 4..InfF)
      refute_equal rany2, [1..2, 4..InfF]
      refute_equal [1..2, 4..InfF], rany2
      refute_equal Rangeary(?a..), [?a..InfP] # This raises the uncapturable SyntaxError for Ruby 2.5 and before anyway.
      refute_equal [?a..InfP], Rangeary(?a..)

      # Ruby 2.7 Beginless Range
      refute_equal [..?z],     Rangeary(InfN..?z)
    end

    def test_brackets
      # Square Brackets
      ar = [-6..-1, 2..5, 8..9]
      rs = Rangeary.new(*ar)
      assert_equal ar[-1],   rs[-1]
      assert_equal ar[1..2], rs[1..2]
      assert_equal ar[0,2],  rs[0,2]
      assert_equal RangeExtd::NONE, RaN[0]
      assert_nil                    RaN[1]
    end


    def test_conjunctionRangeExtd
      r38  = RangeExtd(3..8,  :exclude_begin => 1)
      r38e = RangeExtd(3...8, :exclude_begin => 1)
      r39  = RangeExtd(3..9,  :exclude_begin => 1)
      r39e = RangeExtd(3...9, :exclude_begin => 1)
      r58  = RangeExtd(5..8,  :exclude_begin => 1)
      r58e = RangeExtd(5...8, :exclude_begin => 1)
      r59  = RangeExtd(5..9,  :exclude_begin => 1)
      r59e = RangeExtd(5...9, :exclude_begin => 1)

      # Lower exclusive
      assert_nil           conjRE(1..3,  5..9).begin
      assert_nil           conjRE(1...5, 5..9).begin
      assert_nil           conjRE(1..5,  r59).begin
      assert_nil           conjRE(1...5, r59).begin
      assert_equal (5..5), conjRE(1..5,  5..9)

      # Lower overlap
      assert_equal (5..8), conjRE(3..8,  5..9)
      assert_equal (5...8),conjRE(3...8, 5..9)
      assert_equal r58,    conjRE(3..8,  r59)
      assert_equal r58e,   conjRE(3...8, r59)

      assert_equal (5..9), conjRE(3..9,  5..9)
      assert_equal (5...9),conjRE(3...9, 5..9)
      assert_equal (5...9),conjRE(3..9,  5...9)
      assert_equal (5...9),conjRE(3...9, 5...9)

      # Inclusive
      assert_equal (3..8), conjRE(3..8,  2..9)
      assert_equal (3...8),conjRE(3...8, 2..9)
      assert_equal r38,    conjRE(r38,   2..9)
      assert_equal r38e,   conjRE(r38e,  2..9)

      # Identical but boundaries
      assert_equal (3..8), conjRE(3..8,  3..8)
      assert_equal (3...8),conjRE(3...8, 3..8)
      assert_equal r38,    conjRE(r38,   3..8)
      assert_equal r38e,   conjRE(r38e,  3..8)
      assert_equal (3...8),conjRE(3..8,  3...8)
      assert_equal (3...8),conjRE(3...8, 3...8)
      assert_equal r38e,   conjRE(r38,   3...8)
      assert_equal r38e,   conjRE(r38e,  3...8)
      assert_equal r38,    conjRE(3..8,  r38)
      assert_equal r38e,   conjRE(3...8, r38)
      assert_equal r38,    conjRE(r38,   r38)
      assert_equal r38e,   conjRE(r38e,  r38)
      assert_equal r38e,   conjRE(3..8,  r38e)
      assert_equal r38e,   conjRE(3...8, r38e)
      assert_equal r38e,   conjRE(r38,   r38e)
      assert_equal r38e,   conjRE(r38e,  r38e)

      # Higher overlap
      assert_equal (5..8), conjRE(5..9,  3..8)
      assert_equal (5...8),conjRE(5..9,  3...8)
      assert_equal r58,    conjRE(r59,   r38)
      assert_equal r58e,   conjRE(r59e,  r38e)
      assert_equal r58,    conjRE(r59e,  r38)

      assert_equal (3..8), conjRE(3..9,  3..8)
      assert_equal (3..8), conjRE(3...9, 3..8)
      assert_equal (3...8),conjRE(3..9,  3...8)
      assert_equal (3...8),conjRE(3...9, 3...8)
      assert_equal r38,    conjRE(r39,   3..8)
      assert_equal r38,    conjRE(r39e,  3..8)
      assert_equal r38e,   conjRE(r39,   3...8)
      assert_equal r38e,   conjRE(r39e,  3...8)
      assert_equal r38,    conjRE(r38,   3..9)
      assert_equal r38e,   conjRE(r38e,  3..9)
      assert_equal r38,    conjRE(r38,   3...9)
      assert_equal r38e,   conjRE(r38e,  3...9)

      # Higher exclusive (almost)
      assert_equal (5..5), conjRE(5..9,  1..5)
      assert_nil           conjRE(5..9,  1...5).begin
      assert_nil           conjRE(r59,   1..5).begin
      assert_nil           conjRE(r59,   1...5).begin

      # Higher exclusive (almost)
      assert_nil           conjRE(5..9,  1..3).begin
      assert_nil           conjRE(r59,   1...4).begin

      # String
      assert_equal (?d..?f), conjRE(?a..?f, ?d..?z)

      # Empty
      assert_equal RangeExtd::NONE, conjRE(RangeExtd(1,1,T,T), 0..8)
      assert_equal RangeExtd::NONE, conjRE(0..8, RangeExtd(1,1,T,T))
      assert_equal RangeExtd::NONE, conjRE(RangeExtd::NONE, ?a..?d)
      assert_equal RangeExtd::NONE, conjRE(?a..?d, RangeExtd::NONE)

      # Invalid
      assert_raises(RangeError){ conjRE(true..true, true..true) } # => invalid parameter for RangeExtd
      assert_raises(TypeError){ conjRE(1..5, ?a..?d) }

      assert_equal RangeExtd(24...25,T), conjRE(RangeExtd(24..26,T), 24...25)
    end


    def test_conjunctionRangeary
      inf = Float::INFINITY

      r21e    = RangeExtd(21...22,  :exclude_begin => 1)
      r24e25e = RangeExtd(24...25,  :exclude_begin => 1)
      r24e_26 = RangeExtd(24..26,   :exclude_begin => 1)
      r1 = Rangeary.new(        3..8,    12...15,17...19, 20..22,  r24e_26,28..32, 33..inf)
      r2 = Rangeary.new(-inf..1, 4..6,8..12, 14..18, 19...20,r21e, 24...25)
      rc12     = Rangeary.conjunction(r1, r2)
      rc12tobe = Rangeary(    4..6,8..8, 12..12,14...15,17..18,r21e,r24e25e)
      assert_equal (?d..?f), conjRE(?a..?f, ?d..?z)
      assert_equal rc12tobe, rc12
      rcab     = Rangeary(?x..?z).conjunction(?a..?c)
      assert_equal RangeExtd::NONE, rcab[0]
      assert_equal 1, rcab.size
      assert          rcab.empty_element?
      assert          rcab.null_element?
      assert          rcab.null?

      hsinff = {negative: -InfF, positive: InfF}
      assert_raises(ArgumentError){ r1 * Rangeary(RangeExtd::ALL) }  # Inconsistent given infinities: comparison of Float with RangeExtd::Infinity failed  # used to be fine up to Rangeary Ver.1
      r2 = r1 * Rangeary(-inf..nil)  # Positive infinity (=Float::INFINITY) is inherited and preserved from r1
      assert_equal r1,     r2
      assert_equal hsinff, r2.infinities  # Hash == HashInf
      # reverse
      r2 = Rangeary(-inf..nil) * r1
      assert_equal r1,     r2
      assert_equal hsinff, r2.infinities

      r4 = r1 * Rangeary(..nil)
      assert_equal r1,     r4
      assert_equal hsinff.merge({negative: nil}), r4.infinities

      assert_equal RaN, r1 * RaN
      assert_equal RaN, r1 * RaN
      assert_equal RaN, RaA * RaN
      assert_equal RaN, RaN * r1
      assert_equal RaN, RaN * RaN
      assert_equal RaA, RaA * RaA

      # Eendless Range since Ruby 2.6
      _ = 7..nil rescue return # before Ruby 2.6
      r3 = Rangeary(1...9, 12..nil)
      assert_equal [8...9, 12...13], r3 * Rangeary(8...13)

      # Begindless Range since Ruby 2.7
      r4 = Rangeary(nil..?e, ?t..?y)
      assert_equal [?d..?e, ?t..?t], r4 * Rangeary(?d..?t)

      # preserves infinities?
      assert_equal([nil..],  ~(RaA * RaN))
      hsinf = {negative: ?d, positive: ?q}
      r5 = Rangeary(?d..?g, **hsinf) * RaN
      assert_equal hsinf, r5.infinities
      assert_equal([hsinf[:negative]..hsinf[:positive]], ~r5)
    end	# def test_conjunctionRangeary


    def test_comjunction
      t = true
      inf = Float::INFINITY

      assert_equal Rangeary(12..12), Rangeary.conjunction(8..12, 12..15)
      assert_equal Rangeary(-inf...12, RangeExtd(12,inf,t)), Rangeary.conjunction(8..12, 12..15).negation
      assert_equal Rangeary(8..15), Rangeary.disjunction(8..12, 12..15)
      assert_equal Rangeary(8...12, RangeExtd(12,15,t)), Rangeary.disjunction(8..12, 12..15).conjunction( Rangeary.conjunction(8..12, 12..15).negation )
      assert_equal Rangeary(8...12, RangeExtd(12,15,t)), Rangeary.exclusive_disjunction(8..12, 12..15)
      assert_equal Rangeary(8...12, RangeExtd(12..15,t)), Rangeary(8..12).xor(12..15)

      assert_equal Rangeary(12..12, 14..15), Rangeary.conjunction(Rangeary(8..12, 14..15), 12..15)
      assert_equal Rangeary(-inf...12, RangeExtd(12...14,t), RangeExtd(15,inf,t)), Rangeary(12..12, 14..15).negation
      assert_equal Rangeary(-inf...12, RangeExtd(12...14,t), RangeExtd(15,inf,t)), Rangeary.conjunction(Rangeary(8..12, 14..15), 12..15).negation
      assert_equal Rangeary(8...12, RangeExtd(12...14,t)), Rangeary(8..12,14..15).xor(12..15)
      assert_equal Rangeary(RangeExtd::NONE), Rangeary.conjunction(RangeExtd(24..26,t), 24...25)

      # Eendless Range since Ruby 2.6
      _ = 7..nil rescue return # before Ruby 2.6
      r3 = Rangeary(9..11)
      assert_equal [8..nil],  r3 + Rangeary(8..nil)
      refute_equal [8..InfF], r3 + Rangeary(8..nil)

      # Begindless Range since Ruby 2.7
      r4 = Rangeary(9..11)
      assert_equal [nil..12],  r4 + Rangeary(nil..12)
      assert_equal [nil..12], Rangeary(nil..12) + r4
      assert_equal [nil..11], Rangeary(nil..9) + r4
    end


    def test_xor
      t = true
      inf = Float::INFINITY
      r21e22e = RangeExtd(21...22,  :exclude_begin => 1)
      #r24e25e = RangeExtd(24...25,  :exclude_begin => 1)
      r24e_26 = RangeExtd(24..26,   :exclude_begin => 1)

      r1 = Rangeary.new(        3..8,    12...15,17...19, 20..22,  r24e_26,28..32, 33..inf)
      r2 = Rangeary.new(-inf..1, 4..6,8..12, 14..18, 19...20,r21e22e,24...25)
      rc12     = Rangeary.exclusive_disjunction(r1, r2)
      rc12tobe = Rangeary(-inf..1, 3...4, RangeExtd(6...8,t), RangeExtd(8...12,t), RangeExtd(12...14,t), RangeExtd(15...17), RangeExtd(18..22,t), 24..26, 28..32, 33..inf)
      assert_equal rc12tobe, rc12
      # Actual: <Rangeary:[-Infinity..1, 3...4, 6<...8, 8<...12, 12<...14, 15...17, 18<..22, 24..26, 28..32, 33..Infinity]>

      assert_equal r1,   r1.disjunction(RaN)
      assert_equal true, r1.conjunction(RaN).null?
      #assert_equal Rangeary(RangeExtd::ALL), r1.conjunction(RaN).negation  # this used to be the case in up to Rangeary Ver.1
      assert_equal [-InfF..InfF],      r1.conjunction(RaN).negation
      assert_equal r1,                 r1 * (r1.conjunction(RaN).negation)
      assert_equal r1,  r1 ^ RaN
      assert_equal r1,  RaN ^ r1
      assert_equal RaN, RaN ^ RaN
      assert_equal RaA, RaA ^ RaN
      assert_equal RaA, RaN ^ RaA
      assert_equal RaN, RaA ^ RaA
    end	# def test_xor


    def test_negation
      inf = Float::INFINITY
      assert_equal Rangeary(-inf...3,RangeExtd(8..inf,1)), Rangeary(3..8).negation
      assert_equal Rangeary(-inf...12, RangeExtd(12...14,T), RangeExtd(15,inf,T)), ~Rangeary(12..12, 14..15)

      assert_equal([..nil],  ~RaN)
      # assert_equal Rangeary(RangeExtd::ALL), ~RaN  # used to be the case up to Rangeary Ver.1

      # Eendless Range since Ruby 2.6 plus Beginless Range since Ruby 2.7
      _ = 7..nil rescue return # before Ruby 2.6
      assert_equal Rangeary(-InfF...8), ~Rangeary(8...nil)
      assert_equal Rangeary(-InfF..8),  ~Rangeary(RangeExtd(8..nil, exclude_begin: true))
      assert_equal Rangeary(nil...?a),  ~Rangeary(?a..nil)
      assert_equal Rangeary(RangeExtd(7..InfF, T)), ~Rangeary(-InfF..7)
      assert_equal Rangeary(RangeExtd(7..InfF, T)), ~Rangeary(..7)
      assert_equal Rangeary(?c..nil),               ~Rangeary(...?c)
      assert_equal Rangeary(RangeExtd::NONE), ~Rangeary(RangeExtd::ALL)
      assert_equal Rangeary(RangeExtd::NONE), ~Rangeary(InfN...InfP)
      assert_equal Rangeary(RangeExtd::NONE), ~Rangeary(..nil)
      assert_equal Rangeary(RangeExtd::NONE), ~Rangeary(...nil)
      assert_equal Rangeary(RangeExtd::NONE), ~Rangeary(RangeExtd(...nil, true))

      assert_equal [nil..], ~Rangeary(RangeExtd::NONE)
      assert   (~Rangeary(RangeExtd::NONE)).infinities.status_is_nil?(:positive)
      assert  (~~Rangeary(RangeExtd::NONE)).infinities.status_is_nil?(:positive)
      assert  (~~Rangeary(RangeExtd::NONE)).infinities.status_is_nil?(:negative)

      r3 = ~Rangeary(RangeExtd(6...6, true))
      assert_equal [-InfF..InfF], r3
      assert  r3.infinities.guessed?(:positive), "Status should be guessed for #{r3.infinities.inspect}"
      r4 = ~Rangeary(InfF..InfF) # meaningless Range
      assert_equal [nil..], r4   # Because Float information is lost for the meaningless Rnage
      assert  r4.infinities.status_is_nil?(:positive)
      r5 = ~Rangeary(RangeExtd(?g...?g, true))
      assert_equal [nil..], r5
     # assert_equal RangeExtd::ALL, r5, "At the moment, (nil..)==RangeExtd::ALL (which is subject to change in the future?)"
      assert  r5.infinities.guessed?(:positive)

      assert_equal Rangeary(?a...?d, ?f..?z),   ~Rangeary(?d...?f, negative: ?a, positive: ?z)
      assert_equal Rangeary(?d...?f),          ~~Rangeary(?d...?f, negative: ?a, positive: ?z)
      assert_equal Rangeary(?a...?d, ?f..?z), ~~~Rangeary(?d...?f, negative: ?a, positive: ?z)

      r2  =  Rangeary(6...8)
      r2n = ~r2
      assert       r2.infinities.status_is_a?(:guessed, :positive)
      assert       r2.infinities.guessed?(:positive)
      refute      r2n.infinities.definite?(:positive)
      assert      r2n.infinities.guessed?(:positive), "Negated Range without an explicit infinity should have a status of :guessed"
    end	# def test_negation


    def test_posinega
      inf = RangeExtd::Infinity::POSITIVE
      assert_equal Rangeary(?a...?d, ?x..?z),  ~Rangeary(?d...?x, :negative => ?a, :positive => ?z)
      assert_equal Rangeary(?a...?d, ?x..nil), ~Rangeary(?d...?x, :negative => ?a) 
      assert_raises(ArgumentError){ Rangeary(?t..?z,  :negative => -Float::INFINITY) }
      #TeeIO.suppress_io{|iorw|  # Inconsistent negative infinities are found: ["a", -Infinity] (=> a is used)
        assert_raises(ArgumentError){ Rangeary(1...8.5, :negative => ?a) }
      #}
      assert_raises(ArgumentError){ Rangeary(1...8.5, negative: InfP, positive: -InfF) }
      assert_raises(ArgumentError){ Rangeary(1...8.5, positive: inf) }

      ra = rb = rc = rd = re = rf = rg = nil
      ra = Rangeary(?d...?f,      :negative => ?a)
      rb = Rangeary(?g..?h, ?j...?m)
      rc = ra + rb
      rd = rb | ra
      re = Rangeary(?e...?k, :positive => ?z)
      rf = rd & re
      rg = re & rd

      assert_equal   ?a, ra.infinities[:negative]
      assert_nil         ra.infinities[:positive]
      assert_equal   ?a, rc.infinities[:negative]
      assert_nil         rc.infinities[:positive]
      assert_equal   ?a, rd.infinities[:negative]
      assert_nil         rd.infinities[:positive]
      assert_equal   ?a, rf.infinities[:negative]
      assert_equal   ?z, rf.infinities[:positive]
      assert_equal   ?a, rg.infinities[:negative]
      assert_equal   ?z, rg.infinities[:positive]

      # Demonstrations of how to change infinities.
      rn = Rangeary(5...9, negative: 1, positive: nil)  # Without positive, it becomes +Infinity
      ran1 = Rangeary(1...5, 9..)
      rn_inv = ~rn
      assert_equal ran1, rn_inv

      # The way to change only the infinity; basically, just Rangeary.new is suffice
      # because the option has always the priority.
      ri = Rangeary(rn_inv, negative: -InfF)
      hsinf = {:negative=>-InfF, :positive=>nil}
      assert_equal hsinf, ri.infinities  # Hash == HashInf
      ran2 = Rangeary(-InfF...1, 5...9)
      ri_inv = ~ri
      assert_equal ran2, ri_inv
    end	# def test_posinega


    def test_cover
      r1 = Rangeary(?a..?g, ?m..?z)
      assert   r1.cover?('pp')
      assert !(r1 === 'pp')
      assert !(r1.include_element?('pp'))
      assert !(r1.member_element?('pp'))

      assert ! Rangeary(?a..?g, ?m..?o, ?r..?z).cover?(?h)
      assert !(r1 === RangeExtd(?a..?g))

      assert  !RaN.cover?('pp')
      assert  !RaN.cover?(5)
    end

    def test_each
      # Each
      rs = Rangeary.new(-6..-1, 2..5, 8..9)
      rs.each do |i|
        assert_equal (-6..-1), i
        break
      end
    end	# def test_each

    def test_each_element
      # Each_Element
      rs = Rangeary.new(-6..-1, 2..5, 8..9)
      rs.each_element do |i|
        assert_equal(-6, i)
        break
      end

      n = 0
      rs.each_element do |i|
        n += 1
      end
      assert_equal n, 6+4+2
    end	# def test_each_element


    def test_empty_element
      assert !Rangeary(5..8).empty_element?
      assert  Rangeary(RangeExtd::NONE, RangeExtd(3...3,T)).empty_element?
    end	# def test_empty_element


    def test_flatten_element
      assert [6,7,11], Rangeary(RangeExtd(10...12,T), RangeExtd(5...8,T)  ).flatten_element
      assert_raises(TypeError){ Rangeary( 10...12,    RangeExtd(5.0...8,T)).flatten_element }
    end	# def test_flatten


    def test_iteratable
      assert  Rangeary(?d..?n, RangeExtd(?a..?c,T), ?p..?z).iteratable?
      assert  Rangeary(5...8, RangeExtd(1..3,T), 8...15).iteratable?
      assert !Rangeary(RangeExtd(1..3,T), 6.0...15).iteratable?
    end	# def test_iteratable


    def test_size_element
      assert_equal 2, Rangeary(1...4, RangeExtd(6...9,T)).size
      assert_equal 5, Rangeary(1...4, RangeExtd(6...9,T)).size_element
    end	# def test_size_element


    def test_hash
      assert_equal Rangeary(1...4).hash,  Rangeary(1...4).hash
      assert_equal Rangeary(?a..?c).hash, Rangeary('A'.downcase..?c).hash
      assert_equal Rangeary(1...4, RangeExtd(6...9,T)).hash, Rangeary(1...4, RangeExtd(6...9,T)).hash
      assert(Rangeary(1...4).hash != (1...4).hash)
      assert(Rangeary(1...4).hash != Rangeary(1..4).hash)
      assert(Rangeary(1...4, RangeExtd(6...9,F)).hash != Rangeary(1...4, RangeExtd(6...9,T)).hash)
    end	# def test_hash


    def test_flatten_no_rangeary
      assert_equal [RaN],      Rangeary.flatten_no_rangeary(RaN)
      assert_equal [RaN, RaN], Rangeary.flatten_no_rangeary(RaN, RaN)
      assert_equal [RaN, RaN], Rangeary.flatten_no_rangeary([[RaN, RaN]])
      rang = Rangeary(1..2, 5...6)
      assert_equal [RaN, (2..4), RaN, RaE(5..9), rang], Rangeary.flatten_no_rangeary([[RaN, (2..4), RaN], [[RaE(5..9)], rang]])
    end

    def test_in_document
      assert  Rangeary(RangeExtd(1,"<...",4), 5...8).equiv?(Rangeary(2..3, 5..7))      # => true
      assert_equal 33, Rangeary(2...4, 5..6, 8..9).flatten_element.reduce(:+)   # => 33

      r1 = RangeExtd(?a...?d, true) # => a<...d
      ra = Rangeary(?g..?h, r1)     # => [a<...d, g..h]
      assert_equal r1, ra[0]
      assert_equal ?g..?h, ra[1]
      assert_equal  2, ra.size
      assert_equal ?a, ra.begin
      assert_equal RangeExtd::NONE, Rangeary(RangeExtd::NONE)[0]
      assert_equal RangeExtd::NONE, Rangeary(RangeExtd::NONE, RangeExtd::NONE)[0]
      assert_equal 1, Rangeary.new(RangeExtd::NONE, RangeExtd::NONE).size
      assert  Rangeary(RangeExtd::NONE, RangeExtd::NONE).empty_element?
      assert_equal [1...7], Rangeary(RangeExtd::NONE, 1..5, 3...7)  # => [1...7]
      assert_raises(ArgumentError){ Rangeary(true..true) } # => invalid parameter for RangeExtd

      #assert_equal %w(b c g h), ra.to_a
      assert_equal [RangeExtd(?a...?d,T), ?g..?h], ra.to_a
      assert  !ra.cover?("a")
      assert   ra.cover?("b")                # => true
      assert_equal "h", ra.end
      s=''
      ret = ra.each do |i|
        s+=i.begin
      end    # => self ( "ag" => STDOUT )
      assert_equal s, "ag"
      assert_equal ra, ret
      s=''
      ret = ra.each_element do |i|
        s+=i
      end    # => self ( "bcgh" => STDOUT )
      assert_equal s, "bcgh"
      assert_equal ra, ret

      rb = Rangeary(6...9, 2..4)    # => [2..4, 6...9]
      assert_equal Rangeary(6...9, 2..4), rb
      assert_equal [2..4, 6...9], rb.to_a
      assert_equal [2..4, 6...9], rb
      assert_equal rb, [2..4, 6...9]
      assert_equal [2...9], rb + Rangeary(3..7)           # => [2...9]
      assert_equal [2...3, RangeExtd(7,'<...',9)], rb - Rangeary(3..7)
      assert_equal [4..4, 8...9], rb * Rangeary(4..5, 8..10)
      assert_equal [-Float::INFINITY...2, RangeExtd(4,'<...',6), 9..Float::INFINITY], rb.negation

      assert !Rangeary(RangeExtd::NONE).empty?          # => false
      assert  Rangeary(RangeExtd::NONE).empty_element?  # => true

      assert !(Rangeary(r1) === r1)

      assert   [2..4, 6..8] == Rangeary(2..4, 6..8)
      assert   [2..4, 6..8] == Rangeary(6..8, 2..4)
      assert !([6..8, 2..4] == Rangeary(6..8, 2..4))

      assert_equal 2, Rangeary(1..3, 5..8).size          # => 2
      assert_equal 7, Rangeary(1..3, 5..8).size_element  # => 7

      ### section of Infinities (main doc)
      ran1 = nil..Float::INFINITY
      r1 = Rangeary(ran1).conjunction( RangeExtd::NONE )
      assert_equal RaN, r1
      r2 = r1.negation
      assert_equal Rangeary(ran1), r2
      assert_equal Rangeary(ran1), ~(Rangeary(nil..Float::INFINITY) * RangeExtd::NONE)

      ran2 = ("f".."k")
      rae1 = RaE("k"..nil, true)
      assert_equal Rangeary("d"..."f", rae1), Rangeary(ran2, negative: "d").negation

      r3 =  Rangeary(ran2, negative: "d")
      r4 = ~r3
      assert_equal Rangeary("d"..."f", rae1), r4
      assert_equal "d", r4.infinities[:negative]
      assert            r4.infinities.definite?(:negative)
      assert_nil        r4.infinities[:positive]
      assert            r4.infinities.guessed?(:positive)
      err = assert_raises(ArgumentError){ Rangeary(r4, positive: "t") } # => specified/inherited positive infinity ("t") is not large enough or inconsistent: (<=> nil)
      assert_match(/not large enough or inconsistent\b/, err.message)
      r6 =   Rangeary(r3, positive: "t")  # OK: positive infinity is set.
      assert_equal "d", r6.infinities[:negative]
      assert            r6.infinities.definite?(:negative)
      r7 =  ~r6
      assert_equal Rangeary("d"..."f", RangeExtd("k".."t", true)), r7
      assert_equal r4[0],  r7[0]
      refute_equal r4[1],  r7[1]

      assert_equal Rangeary(-InfF...7), Rangeary(7..).negation
      assert_equal Rangeary(7..),       Rangeary(7..).negation.negation
      assert_equal Rangeary(...7),      Rangeary(7.., negative: nil).negation
      assert_equal Rangeary(...7),   Rangeary(..nil).conjunction(Rangeary(7..)).negation
      assert_equal Rangeary(...7), ~(Rangeary(..nil) * Rangeary(7..))

    end	# def test_in_document


  end	# class TestUnitFoo < MiniTest::Unit::TestCase

#end	# if $0 == __FILE__


