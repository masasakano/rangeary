# -*- encoding: utf-8 -*-

$stdout.sync=true
$stderr.sync=true
# print '$LOAD_PATH=';p $LOAD_PATH
arlibrelpath = []
arlibbase = %w(rangeary)

arlibbase.each do |elibbase|
  #elibbase

  arAllPaths = []
  er=nil
  pathnow = nil
  (['../lib/', 'lib/', ''].map{|i| i+elibbase+'/'} + ['']).each do |dir|
    # eg., pathcand = %w(../lib/rangesmaller/ lib/rangesmaller/ rangesmaller/) + ['']
    begin
      s = dir+File.basename(elibbase)
      arAllPaths.push(s)
#print "Trying: "; puts s
      require s
      pathnow = s
      break
    rescue LoadError => er
    end
  end	# (['../lib/', 'lib/', ''].map{|i| i+elibbase+'/'} + '').each do |dir|

  if pathnow.nil?
    warn "Warning: All the attempts to load the following files have failed.  Abort..."
    warn arAllPaths.inspect
    warn " NOTE: It may be because a require statement in that file failed, 
rather than requiring the file itself.
 Check with  % ruby -r#{File.basename(elibbase)} -e p
 or maybe add  env RUBYLIB=$RUBYLIB:`pwd`"
    # p $LOADED_FEATURES.grep(/#{Regexp.quote(File.basename(elibbase)+'.rb')}$/)
    raise er
  else
#print pathnow," is loaded!\n"
    arlibrelpath.push pathnow
  end
end	# arlibbase.each do |elibbase|

print "NOTE: Library relative paths: "; p arlibrelpath
print "NOTE: Library full paths:\n"
arlibbase.each do |elibbase|
  p $LOADED_FEATURES.grep(/#{Regexp.quote(File.basename(elibbase)+'.rb')}$/)
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
      hs = rn.instance_eval{_get_infinities [(77..78)], guess_strict: false}
      assert_equal(-InfF, hs[:negative])
      assert_equal( InfF, hs[:positive])
      hs = rn.instance_eval{_get_infinities [(77...InfF)], guess_strict: false}
      assert_equal(-InfF, hs[:negative])
      assert_equal( InfF, hs[:positive])
      hs = rn.instance_eval{_get_infinities [(-InfF..78)], guess_strict: false}
      assert_equal(-InfF, hs[:negative])
      assert_equal( InfF, hs[:positive])
      hs = rn.instance_eval{_get_infinities [(77...InfF)], guess_strict: true}
      assert_nil  hs[:negative]
      assert_equal( InfF, hs[:positive])
    end

    def test_comparable_end
      assert_equal 3,    Rangeary.comparable_end(0...3)
      assert_equal InfF, Rangeary.comparable_end(0...Float::INFINITY)
      assert_nil         Rangeary.comparable_end(RangeExtd::NONE)
      _ = nil..nil  rescue return  # nil..nil may raise Exception in some Ruby versions
      assert_nil         Rangeary.comparable_end(nil..nil)
      _ = nil...nil rescue return  # nil..nil may raise Exception in some Ruby versions
      assert_nil         Rangeary.comparable_end(nil...nil)
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
    end	# def test_sort_ranges

    def test_new
      assert_equal [2..5, 8..8], Rangeary.new(RangeExtd(8..8), 2..5).to_a
      assert_equal [2..5, 8..8], Rangeary(RangeExtd(8..8), 2..5).to_a
      rs = Rangeary.new([3..5, -6..-1, -4..-3, 2..4, 8..8])
      assert_equal [-6..-1, 2..5, 8..8], rs.to_a

      assert_raises(NoMethodError){ Rangeary.new(3..5).reverse }	# => undefined method `reverse' for <Rangeary:[3..5]>:Rangeary
      assert_raises(RuntimeError){  Rangeary.new(3..5).reverse! }	# => can't modify frozen Rangeary
      assert_raises(ArgumentError){ Rangeary.new(3..5, nil..nil) }	# => invalid parameter for RangeExtd, hence for Rangeary (nil..nil).
      assert_raises(ArgumentError){ Rangeary.new(3..5, 3..1) }	# => invalid parameter for RangeExtd
      assert_raises(ArgumentError){ Rangeary.new(3..5, 4...4) }	# => invalid parameter for RangeExtd
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

      assert_equal [r1], Rangeary(r1,(-InfF..-InfF))
      assert_equal [r1], Rangeary(r1,(InfF..InfF))
    end	# def test_new_infinity

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

      # Eendless Range since Ruby 2.6
      _ = 7..nil rescue return # before Ruby 2.6
      assert_equal [-6..-1, 2..5, 8...nil], rs+(8...nil)
      assert_equal [-6..-1, 2..5, 8..nil],  Rangeary(8..nil)+rs
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
      assert_equal [8..nil],  rr-(6...8)
      assert_equal [6...7],   rr-(7...nil)
      rr = Rangeary(6..9, 8..nil)
      assert_equal [8..nil],  rr-(6...8)
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

      assert_equal rany, [4..InfF]
      assert_equal rany, [4...InfF]
      assert_equal [4...InfF], rany
      assert_equal rany2, Rangeary(1..2, 4..InfF)
      assert_equal rany2, [1..2, 4..InfF]
      assert_equal [1..2, 4..InfF], rany2
      assert_equal Rangeary(?a..), [?a..InfP]
      assert_equal [?a..InfP], Rangeary(?a..)
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

      assert_equal r1,  r1 * Rangeary(RangeExtd::ALL)
      assert_equal r1,  Rangeary(RangeExtd::ALL) * r1

      assert_equal RaN, r1 * RaN
      assert_equal RaN, r1 * RaN
      assert_equal RaN, RaN * r1
      assert_equal RaN, RaN * RaN
      assert_equal RaA, RaA * RaA

      # Eendless Range since Ruby 2.6
      _ = 7..nil rescue return # before Ruby 2.6
      r3 = Rangeary(1...9, 12..nil)
      assert_equal [8...9, 12...13], r3 * Rangeary(8...13)
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
      assert_equal [8..InfF], r3 + Rangeary(8..nil)
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
      assert_equal Rangeary(RangeExtd::ALL), r1.conjunction(RaN).negation
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

      assert_equal Rangeary(RangeExtd::ALL), ~RaN

      # Eendless Range since Ruby 2.6
      _ = 7..nil rescue return # before Ruby 2.6
      assert_equal Rangeary(-InfF...8), ~Rangeary(8...nil)
      assert_equal Rangeary(-InfF..8),  ~Rangeary(RangeExtd(8..nil, exclude_begin: true))
      assert_equal Rangeary(InfN...?a), ~Rangeary(?a..nil)
    end	# def test_negation


    def test_posinega
      inf = RangeExtd::Infinity::POSITIVE
      assert_equal Rangeary(?a...?d, ?x..?z),  ~Rangeary(?d...?x, :negative => ?a, :positive => ?z)
      assert_equal Rangeary(?a...?d, ?x..inf), ~Rangeary(?d...?x, :negative => ?a) 
      assert_raises(ArgumentError){ Rangeary(?t..?z,  :negative => -Float::INFINITY) }
      assert_raises(ArgumentError){ Rangeary(1...8.5, :negative => ?a) }
                                _ = Rangeary(1...8.5, :positive => inf)	# => No error.

      ra = Rangeary(?d...?f,      :negative => ?a)
      rb = Rangeary(?g..?h, ?j...?m)
      rc = ra + rb
      rd = rb | ra
      re = Rangeary(?e...?k, :positive => ?z)
      rf = rd & re
      rg = re & rd
      assert_equal   ?a, ra.infinities[:negative]
      assert_equal  inf, ra.infinities[:positive]
      assert_equal   ?a, rc.infinities[:negative]
      assert_equal  inf, rc.infinities[:positive]
      assert_equal   ?a, rd.infinities[:negative]
      assert_equal  inf, rd.infinities[:positive]
      assert_equal   ?a, rf.infinities[:negative]
      assert_equal   ?z, rf.infinities[:positive]
      assert_equal   ?a, rg.infinities[:negative]
      assert_equal   ?z, rg.infinities[:positive]
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
    end	# def test_in_document


    ### All false, well, and they should be.
    # def test_equal
    #   # Plus
    #   assert_equal RangeExtd(1..3), RangeExtd(1...4)
    #   assert_equal Rangeary(1..3),  Rangeary(1...4)
    # end

  end	# class TestUnitFoo < MiniTest::Unit::TestCase

#end	# if $0 == __FILE__


