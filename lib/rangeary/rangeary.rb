# -*- coding: utf-8 -*-

## Load required files.
# require "range_extd/range_extd"
err1st = nil
req_files = %w(lib/range_extd/range_extd)
req_files.each do |req_file|
  while ! req_file.empty?
    begin
      require req_file 
    rescue LoadError => errLoad
      err1st = errLoad if err1st.nil?
      if %r@/@ =~ req_file
        if req_file.sub!(%r@[^/]*/@, '').nil?	# Will search for the next directory down.
          raise
        end
      else
        req_file = ''
        break
      end
    else
      break
    end
  end
  if req_file.empty?
    raise err1st
  end
end	# req_files.each do |req_file|



def Rangeary(*rest)
  Rangeary.new(*rest)
end

# =Class Rangeary < Array
#
# Authors:: Masa Sakano
# License:: MIT
#
# ==Summary
#
# Class to express the multiple ranges.
#
# The library package <tt>range_extd</tt> is required.
# https://rubygems.org/gems/range_extd
#
# An arbitrary number of {Rangeary},
# {RangeExtd}[http://rubygems.org/gems/range_extd] or Range objects (or its
# descendant classes) can be supplied for the constructor {#initialize}.
# Then, an operation of disjunction is performed to get the clean Array of
# RangeExtd, none of which overlaps with the range.
#
# Once it is constructed, {Rangeary} objects are immutable.  Any subsequent
# operations return a new {Rangeary} object.  All but few methods of {Array}
# are inherited, though some methods, such as +push+ do not work because of
# immutability of {Rangeary} objects.
#
class Rangeary < Array
  undef_method :*, :+, :length, :reverse

  # # Hash with the keys of :negative and :positive  => becomes a method in Ver.1
  # attr_reader :infinities

  # Constructor
  #
  # Arbitrary (positive) number of arguments can be given.
  # (r1, [r2, ...])
  #
  # == Algorithm about the infinity
  #
  # In {Rangeary} operations, "infinity" is essential.  Without it, negation
  # could not be definied.  To determine what the positive and negative
  # infinities for the given elements is not a trivial task.
  #
  # Callers can supply user-defined infinity objects for both or either
  # positive and negative infinity and in that case they are accepted
  # as the infinities with the highest priority, though ArgumentError might be
  # issued if they contradict the elements; for example, if a {Rangeary}
  # instance consists of an array of Integer Ranges (RangeExtd) like (3..8),
  # and yet if String "abc" is specified as an infinity, it *contradicts*
  # with the elements in the sense they are not comparable.
  #
  # Alternatively, if the main argument contains one or moer {Rangeary}
  # instances, their infinities are inherited.  If their infinities are not
  # identical to one another, warning may be issued if $VERBOSE is true, and
  # the infinities are selected in the following priority order:
  #
  # 1. If one or more user-supplied infinities are found, the smallest and
  #    largest ones are accepted for the positive and negative infinities.
  #    Note the consistency is not checked; that is, even if one or more of
  #    the provided Range elements from which a {Rangeary} instance is about
  #    to be constructed exceed the infinities, this does not warn. Obviously
  #    subsequent operations may result in some unexpected result. Be warned.
  # 2. Float::INFINITY.
  # 3. RangeExtd::Infinity instances (RangeExtd::Infinity::POSITIVE
  #    and RangeExtd::Infinity::NEGATIVE)
  #
  # If none of the main arguments contains {Rangeary}, the elements of each
  # Range are checked whether they have any comparable Numeric instances
  # like Integer (but not Complex), and if so, Float::INFINITY is set
  # as the infinities.  Otherwise, it falls back to the defaults of
  # the RangeExtd::Infinity instances.
  #
  # The registered infinities for each instance is obtained as a Hash with
  # two keys of +:positive+ and +negative+ with the method {#infinities};
  # for example,
  #
  #   ran.infinities  # => { :positive => RangeExtd::Infinity::POSITIVE,
  #                   #      :negative => "aa" }
  #
  # === Note for Developers about the infinity
  #
  # Instance variable @infinities (Hash) is defined.  The elements can be nil;
  # for example, when only the element is RangeExtd::NONE.
  # So, it is recommended to access with the {#infinities} method rather
  # than accessing @infinities directly.
  #
  # At the moment, the infinities are guessed from the elements of the Range
  # it contains and set (+guess_strict: false+ is given to {#_get_infinities}).
  # If +guess_strict: true+ is given, unless a Range contains literally
  # +Float::INFINITY+ etc, (the element of) @infinities is not set (remains nil).
  # I hope it would work even if +guess_strict: true+ is given.  However,
  # it is not tested properly!  Anyway, to check the consistency of
  # the infinities is desirable, which is performed now, thanks to
  # +guess_strict: false+ given to {#_get_infinities}.
  #
  # @param inarall [Object] An arbitrary number of either {Rangeary}, RangeExtd or Range objects (or its subclasses).
  # @option **opts [Object] :positive Object for positive infinity. In default Float::INFINITY for comparable Numeric or else {RangeExtd::Infinity::POSITIVE}.
  # @option **opts [Object] :negative Object for negative infinity. In default -Float::INFINITY for comparable Numeric or else {RangeExtd::Infinity::NEGATIVE}.
  def initialize(*inarall, **opts)
    @infinities =  { :negative => nil, :positive => nil }

    if inarall.size < 1
      super [RangeExtd::NONE]  # Since Ver.1
      return
      # raise ArgumentError, "wrong number of arguments (#{inarall.size} for 1 or more)."
    end

    # Unfold Rangeary into Array of Arrays and convert Range-s into RangeExtd-s
    in_ranges = inarall.map{|i|
      self.class.send(:is_rangeary_type?, i) ? i.to_a : i
    }.flatten.map{|j|
      if (defined? j.exclude_begin?)
        j
      else
        begin
          RangeExtd.new(j)
        rescue ArgumentError, RangeError  # Just to change the error message.
          raise ArgumentError, "invalid parameter for RangeExtd, hence for Rangeary (#{j.inspect})."
        end
      end
    }

    # NOTE: Simple map(&:infinities) is not ideal here, because map(&:infinities) tries to
    #   supplement when the value is nil.  However, in self.conjunction(), it uses map(&:infinities),
    #   which is not ideal.
    inherited_infs = inarall.find_all{|i| self.class.send(:is_rangeary_type?, i)}.map{ |ec|
      ec.instance_variable_get(:@infinities)
    }

    @infinities = _get_infinities(in_ranges, inherited_infs: inherited_infs, guess_strict: false, **opts) # set @infinities (wherever possible)

    # Call _merge_overlaps
    begin
      arRange = _merge_overlaps( convert2range(in_ranges) )
    rescue => err
      # Trap just to change the type of the exception.
      raise ArgumentError, err.message, err.backtrace
    end

    if arRange.empty?
      raise ArgumentError, 'no significant argument given for Rangeary.'
    end

    ## Setting @infinities[:negative, :positive]
    # set_infinities_legacy(arRange, hsInheritedObj, hsInheritedAry, hsInheritedClass, **opts)

    super(arRange)
    self.freeze
  end	# def initialize(*inarall, **opts)

  alias_method :triple_equals_orig, :=== if ! self.method_defined?(:triple_equals_orig)

  # Returns @infinities where nil values are replaced with something significant.
  #
  # Best guess approach is taken.
  #
  # @return [Hash] keys of :positive and :negative.  The values are never nil.
  def infinities
    defhs = {
      :positive => RangeExtd::Infinity::POSITIVE,
      :negative => RangeExtd::Infinity::NEGATIVE,
    }
    hsret = {}.merge @infinities
    if hsret[:positive].nil?
      return defhs if hsret[:negative].nil?
      hsret[:positive] = (is_num_type?(hsret[:negative]) ?  Float::INFINITY : defhs[:positive])
      return hsret
    end
    if hsret[:negative].nil?
      hsret[:negative] = (is_num_type?(hsret[:positive]) ? -Float::INFINITY : defhs[:negative])
      return hsret
    end

    hsret
  end

  # If self covers the entire range?
  #
  def all?
    rfirst = self[0]
    ((1 == size) &&
     !rfirst.is_none? &&
     (infinities[:negative] == rfirst.begin) &&
     ((infinities[:positive] == rfirst.end) || rfirst.end.nil?) &&  # Ruby 2.6 Endless Range
     (!rfirst.exclude_begin?) &&
     (!rfirst.exclude_end?))
  end

  # True if the inObj is in the range.
  #
  # This method works on the basis of each element, that is,
  # if for any of the {RangeExtd} in the {Rangeary}, {RangeExtd#===}
  # returns true, this will return true.  That means, if the argument is
  # a {Rangeary} (or {Range}) object, this always returns false.
  # Note {#include?} and {#member?} work the same as in the standard {Array}, 
  # whereas {#include_element?} and {#member_element?} are the alias of 
  # this method.
  #
  # See {#cover?}.  The difference between this method and {#cover?} is
  # the same as that in {Range}.
  # @return [Boolean]
  def ===(inObj)
    to_a.each do |ea|
      if ea === inObj
        return true
      end
    end
    return false
  end

  alias_method :include_element?, :===
  alias_method :member_element?, :===


  # @return [Object]  The {RangeExtd#begin} of the first {RangeExtd}.
  def begin()
    if to_a.size > 0
      to_a[0].begin 
    else
      nil	# Should not happen!
    end
  end
  alias_method :begin_element, :begin


  # If inObj is within the ranges, it will return true.
  #
  # See {#===}.
  # The difference between this method and {#===} is
  # the same as that in {Range}.
  def cover?(inObj)
    to_a.each do |ea|
      if ea.cover? inObj
        return true
      elsif (ea.end <=> inObj) == 1
        return false	# No point of carrying on searching.
      end
    end
    return false
  end	# def cover?(inObj)


  # Iterator for each element in the ranges.
  # @return [self]
  def each_element
    each do |er|
      er.each do |ee|
        yield ee
      end
    end
    self
  end


  # If the range defined in this object is empty (as in {Range#empty?}), returns true.
  # 
  def empty_element?
    each do |er|
      if ! er.empty?
        return false
      end
    end
    return true
  end


  # @return [Object]  The {RangeExtd#end} of the last {RangeExtd}.
  def end()
    if to_a.size > 0
      to_a[-1].end
    else
      nil
    end
  end
  alias_method :end_element, :end


  # {Range#equiv?} method, defined in range_extd library, extended to this {Rangeary}.
  #
  # @example
  #    Rangeary(RangeExtd(1,"<...",4), 5...8).equiv?(Rangeary(2..3, 5..7))      # => true
  #
  # @param other [Rangeary]
  def equiv?(other)
    return false if size() != other.size

    self.zip(other).each do |ear|
      if ! ear[0].equiv?(ear[1])
        return false
      end
    end

    true
  end	# def equiv?(other)


  # Returns the first n elements of the entire range, the same as {Range#first}.
  # 
  # If the argument is not given, this simply calls {Range#begin} for the first
  # {RangeExtd}.
  # 
  # If not, and if the elements in the ranges are not discrete, like Float,
  # an exception is raised (see {Range#first}).
  # Note this works on the element basis, being different from {Rangeary#first},
  # which works on the array basis.
  # 
  # @param n [Integer] (Optional) positive.
  # @return [Object] equivalent to {#begin} if no argument is given.
  # @return [Array] Array of the first n elements in the range.
  # @raise [TypeError] if the ranges has no iteration defined, such as, starting from Float.
  def first_element(n=nil)
    if n.nil?
      self.begin()
    elsif n < 0
      raise ArgumentError, "the argument #{n} has to be positive."
    else
      arRet = []
      m = n
      to_a.each do |eachr|
        ar = eachr.first(m)
        arRet += ar
        if arRet.size >= n
          break
        else
          m -= ar.size
        end
      end

      arRet
    end		# if n.nil?
  end	# def first_element(n=nil)


  # Return an array of objects that consist of the ranges.
  #
  # @return [Array]
  # @raise [TypeError] if any of the ranges has no iteration defined, such as, starting from Float.
  # @example
  #    Rangeary(2...4, 5..6, 8..9).to_a      # => [2...4, 5..6, 8..9]
  #    Rangeary(2...4, 5..6, 8..9).flatten   # => [2, 3, 5, 6, 8, 9]
  #    Rangeary(2...4, 5..6, 8..9).flatten.reduce(:+)   # => 33
  def flatten_element
    to_a.reduce([]){|a,b| a+=b.to_a}
  end


  # @return [String]
  def inspect
    "<Rangeary:#{to_a.inspect}>"
  end


  # Can iterate?
  def iteratable?
    begin
      each do |i|
        i.each{break}
      end
    rescue TypeError
      return false
    end
    true
  end	# def iteratable?


  # Returns the last n elements of the entire range, the same as {Range#last}.
  # 
  # If the argument is not given, this simply calls {Range#end} for the last
  # {RangeExtd}.
  # 
  # If not, and if the elements in the ranges are not discrete, like Float,
  # an exception is raised (see {Range#last}).
  # Note this works on the element basis, being different from {Rangeary#last},
  # which works on the array basis.
  # 
  # @param n [Integer] (Optional) positive.
  # @return [Object] equivalent to {#end} if no argument is given.
  # @return [Array] Array of the last n elements in the range.
  # @raise [TypeError] if any of the ranges has no iteration defined, such as, starting from Float.
  def last_element(n=nil)
    if n.nil?
      self.end()
    elsif n < 0
      raise ArgumentError, "the argument #{n} has to be positive."
    else
      arRet = []
      m = n
      to_a.reverse.each do |eachr|
        ar = eachr.last(m)
        arRet = ar + arRet
        if arRet.size >= n
          break
        else
          m -= ar.size
        end
      end

      arRet
    end		# if n.nil?
  end	# def last_element(n=nil)


  # Practically equivalent to {#empty_element?}.
  def null_element?
    each do |er|
      if ! er.null?
        return false
      end
    end
    return true
  end
  alias_method :null?, :null_element?


  # Return the sum of {RangeExtd#size} of all the ranges in the object.
  #
  # @return [Integer]  0 if {RangeExtd::NONE}
  # @return [Float]  Float::INFINITY if one of ranges is open-ended.
  # @return [nil] if any of the range is non-Numeric and not open-ended to the infinity.
  def size_element()
    begin
      to_a.map(&:size).reduce(:+)
    rescue TypeError
      nil
    end
  end


  # ======================= Operators =======================

  # Disjunction of a Rangeary (or RangeExtd or Range) and another
  # 
  # @param r1 [Rangeary, RangeExtd, Range]
  # @param r2 [Rangeary, RangeExtd, Range]
  # @return [Rangeary]
  def self.disjunction(r1, r2)
    self.new(r1, r2)
  end

  # Add (Disjunction) a Rangeary (or RangeExtd or Range)
  #
  # @param inr [Rangeary, RangeExtd, Range]
  # @return [Rangeary]
  def disjunction(inr)
    self.class.new(self, inr)
  end

  alias_method :+, :disjunction
  alias_method :|, :disjunction		# "|" (plus with Object#eql?) in general, but in this case it is identical.

  # Exclusive Disjunction (XOR) with a Rangeary (or RangeExtd or Range)
  # 
  # @param r1 [Rangeary, RangeExtd, Range]
  # @param r2 [Rangeary, RangeExtd, Range]
  # @return [Rangeary]
  def self.exclusive_disjunction(r1, r2)
    Rangeary.new(r1).exclusive_disjunction(r2)
  end

  # Exclusive Disjunction (XOR) with a Rangeary (or RangeExtd or Range)
  # 
  # @param inr [Rangeary, RangeExtd, Range]
  # @return [Rangeary]
  def exclusive_disjunction(inr)
    (disjunction(inr)).conjunction(conjunction(inr).negation)
  end

  alias_method :^,   :exclusive_disjunction
  alias_method :xor, :exclusive_disjunction

  # Subtraction.
  #
  # @param r [Rangeary, RangeExtd, Range]
  # @return [Rangeary] 
  def subtraction(r)
    conjunction( Rangeary.new(r).negation )
  end

  alias_method :-, :subtraction


  # Conjunction.
  #
  # @param r [Rangeary, RangeExtd, Range]
  # @return [Rangeary] 
  def conjunction(r)
    self.class.conjunction(self, r)
  end

  alias_method :&, :conjunction
  alias_method :*, :conjunction


  # Negation (class method).
  # 
  # @param r [Rangeary, RangeExtd, Range]
  # @return [Rangeary] 
  def self.negation(r)
    self.new(r).negation
  end


  # Negation.
  # 
  # @return [Rangeary] 
  def negation()
    if to_a.empty?
      raise "ERROR: No range is defined."	# This should not happen.
    end

    arran = []
    prevend = nil          # if "end" is nil, this is substituted with Infinity.
    prevend_orig = Object  # For Endless Range (Ruby 2.6), this can be nil.
    prevst  = nil
    to_a.each do |eachr|

      # ALL -> NONE
      return Rangeary.new(RangeExtd::NONE, :positive => @infinities[:positive], :negative => @infinities[:negative]) if RangeExtd::ALL == eachr

      # null(NONE) -> ALL
      if eachr.null? 
        begin
          _beg = 1.0 * eachr.begin
          return (-Float::INFINITY..Float::INFINITY)
        rescue TypeError	# XXXX can't be coerced into Float
          return Rangeary.new(infinities[:negative]..infinities[:positive])
        end
      end

      ea_b = eachr.begin
      if (RangeExtd::Infinity::NEGATIVE == ea_b)	# Including -Float::INFINITY and other general negative infinities (nb., [#==] is not commutative!).
        # The existing first range starts from the negative infinity, so skip this one.
      else
        if prevend.nil?
          # The returned first range starts from the negative infinity.
          ran_tmp = normalized_range_for_negation(infinities[:negative], eachr.begin)
          if RangeExtd::NONE != ran_tmp
            # Avoid (inf..inf) type, which would cause RangeError (in Ruby 2.6) anyway.
            arran.push( RangeExtd(ran_tmp, :exclude_end => (! eachr.exclude_begin?)) )
          end
        else
          arran.push( RangeExtd(prevend_orig, eachr.begin, :exclude_begin => (! prevst), :exclude_end => (! eachr.exclude_begin?)) )
          # arran.push( RangeExtd(prevend, eachr.begin, :exclude_begin => (! prevst), :exclude_end => (! eachr.exclude_begin?)) )
        end
      end	# if (eachr.begin == -Float::INFINITY) || (RangeExtd::NONE == eachr.begin)
      prevend_orig = eachr.end
      prevend = _comparable_end(eachr)  # For Ruby-2.6 Endless Range
      prevst  = eachr.exclude_end?
    end		# to_a.each do |eachr|

    if (RangeExtd::Infinity::POSITIVE == prevend)	# Including Float::INFINITY and other general positive infinities (nb., [#==] is not commutative!).
      ## Do nothing
    else
      rbeg = (prevend_orig || prevend)
      ran_tmp = normalized_range_for_negation(rbeg, infinities[:positive])

      if RangeExtd::NONE != ran_tmp
        # Avoid (inf..inf) type, which would cause RangeError (in Ruby 2.6) anyway.
        arran.push( RangeExtd.new(ran_tmp, :exclude_begin => (! prevst)) )
      end
    end

    Rangeary.new(arran, :positive => infinities[:positive], :negative => infinities[:negative])
  end	# def negation()

  alias_method :~@, :negation


  ####################
  # Public class methods
  ####################

  # Conjunction.
  #
  # @param r1 [Rangeary, RangeExtd, Range]
  # @param r2 [Rangeary, RangeExtd, Range]
  # @return [Rangeary]
  def self.conjunction(r1, r2)

    r1 = Rangeary.new(r1) if ! defined? r1.first_element
    r2 = Rangeary.new(r2) if ! defined? r2.first_element
    return Rangeary.new(RangeExtd::NONE) if r1.null? || r2.null?
    return Rangeary.new(r1) if r2.all?
    return Rangeary.new(r2) if r1.all?

    # Getting inherited options (if Rangeary is given) for the later use.
    hsInherited = _validate_select_infinities [r1, r2].map(&:infinities)
    # hsInherited = _best_inherited_infinities [r1, r2].map(&:infinities)

    # Initialisation
    a1 = r1.to_a
    a2 = r2.to_a
    rc = Rangeary.new( RangeExtd::NONE, hsInherited )

    if a1.empty? || a2.empty?
      return rc
    end

    ### Algorithm
    # Conditions: Both a1 and a2 are sorted in order of #begin().
    # a1 is a reference array of RangeExtd
    #  Then, Sigma(i=0..(a2.size-1)) a2[i]*a1[j=0..-1]  => Returned Rangeary
    #
    # In reality, I put some tricks to avoid unnecessary calculation
    # for the sake of the processing speed.  But essentially
    # it is a very simple algorithm.
    #
    last_a1index = 0
    a2.each do |ea2|
      a1.each_with_index do |ea1, ind|
        # A bit of trick just to avoid unnecessary process
        next  if ind < last_a1index	# skip

        # For Ruby-2.6 Endless Range, comparable_end() is employed.
        # Note: this comparison ignores @infinities even if set,
        #   because @infinities may not be defined in the arguments!
        #   Anyway, nothing should be larger than the upper limit (as tested below),
        #   and so this should be fine.
        break if comparable_end(ea2) < ea1.begin	# Completely out of range
        if comparable_end(ea1) < ea2.begin		# Completely out of range
          last_a1index = ind if last_a1index < ind
          next
        end

        # Core - Perform conjunction.
        pq1 = conjunctionRangeExtd(ea1, ea2)	# => Rangeary.conjunctionRangeExtd()
        if ! pq1.empty?
          rc += Rangeary.new(pq1)
          last_a1index = ind
        end
      end	# a1.each_with_index do |ea1, ind|
    end		# a2.each do |ea2|

    rc
  end	# def self.conjunction(r1, r2)


  # Returns the infinity "end" that is comparable.
  #
  # Since Ruby-2.6, the Endless Range is introduced.
  # In Ruby definition, the Endless Range er takes a form of er=(5..nil),
  # and accordingly its "end" (or +last+) is nil:
  #
  #   (5..nil).end  # => nil
  #
  # Then, when you compare the "ends" of two Ranges with the method +<=>+,
  # the result too is nil.
  #
  # In fact, in Ruby, two Range objects are not comparable with eath other,
  # though the operator (or method) +<=>+ is defined for Range:
  #
  #   (3..6) <=> (8..9)  # => nil
  #
  # (Note this default behaviour is not unreasonable given what one compares
  # is equivocal for Ranges, for example, the size, start or end?
  # Indeed Range does not have a method +Range<=+ and hence +<=>+ is meaningless.)
  #
  # Then, when Range#end returns just nil, which should be interpreted
  # as *Endless* in Range's context, it does not matter for Range.
  # However, it is inconvenient for this class, {Rangeary}!
  # The heart of comparison in {Rangeary} is +<=>+ for the begin first,
  # and then the end if the first comparison results in equal.
  #
  # This method converts Range#end into something comparable when possible.
  # More specifically, if the "end" of the given Range is nil, this returns
  # Infinity (either +Float::INFINITY+ or +RangeExtd::Infinity::POSITIVE+),
  # unless both begin and end are nil (RangeExtd::NONE, nil..nil, nil...nil),
  # in which case nil is returned, and else returns simply +Range#end+.
  #
  # @param ran [Range, RangeExtd]
  # @return [Object]
  def self.comparable_end(ran)
    if ran.class.method_defined?(:is_none?) && ran.is_none?
      nil
    elsif ran.end.nil? && ran.begin.nil?
      nil
    elsif ! ran.end.nil?
      ran.end  # Before Ruby 2.6, this is always the case.
    elsif ran.begin.class.method_defined? :to_int
      Float::INFINITY
    else
      RangeExtd::Infinity::POSITIVE
    end
  end

  # Returns the array sorted, based on (1) the begin objects and their boundary state,
  # (2) then the end objects and their boundary state.
  # {RangeExtd::NONE} comes first, if included in the input array.
  #
  # @param ar [<Range, RangeExtd>] Arbitrary number.
  # @return [Array<Range, RangeExtd>]
  def self.sort_ranges(*ar)
    ar.flatten.sort{ |a,b|
      err_msg_ab = "invalid parameter (#{a.inspect} or #{b.inspect})."

      # Since Ruby-2.6, the end can be nil (Endless Range).
      # Before Ruby-2.6, they used to raise Exception (ArgumentError) in Range
      # and so they would not have sneaked in to RangeExtd, either.
      # The following is in that sense meaningful only for Ruby 2.6 and later.
      ends = { :a => a, :b => b }
      ends.each_pair do |k, v|
        ends[k] = comparable_end(v)
      end

      ret = a.begin <=> b.begin
      case ret
      when -1, 1
        ret
      when 0
        a_exc_begin = (a.exclude_begin? rescue false)
        b_exc_begin = (b.exclude_begin? rescue false)
        if (a_exc_begin ^ b_exc_begin)
          if a_exc_begin	# but not b
            1
          else
            -1
          end
        else	# <= (a.exclude_begin? == b.exclude_begin?)
          ret = ends[:a] <=> ends[:b]
          case ret
          when -1, 1
            ret
          when 0
            if (a.exclude_end? && b.exclude_end?)
              0
            elsif a.exclude_end?	# but not b
              -1
            else	# <= b.exclude_end?  but not a
              1
            end		# if (a.exclude_end? && b.exclude_end?)
          when nil
            # This should not happen for Range, let alone RangeExtd.
            # But could be the case for a user-defined class.
            if ends[:a].nil? && ends[:b].nil?
              0
            elsif ends[:a].nil?
              -1
            elsif ends[:b].nil?
               1
            else
              raise(TypeError, err_msg_ab)
            end
          else	# case ret # ends[:a] <=> ends[:b]
            raise(TypeError, err_msg_ab)
          end	# case ret # ends[:a] <=> ends[:b]
        end	# if (a.exclude_begin? ^ b.exclude_begin?)
      when nil	# case ret #(a.begin <=> b.begin)
        if a.begin.nil? && b.begin.nil?
          0
        elsif a.begin.nil?
          -1
        elsif b.begin.nil?
           1
        else
          raise(TypeError, err_msg_ab)
        end
      else	# case ret #(a.begin <=> b.begin)
        raise(TypeError, err_msg_ab)
      end	# case ret #(a.begin <=> b.begin)
    }	# ar.sort{ |a,b|
  end	# def self.sort_ranges(ar)


  ####################
  private
  ####################

  # Called from {Rangeary#initialize}
  def convert2range(inarall)
    inarall.flatten.map{|i|
      if defined? i.first_element
        i.to_a
      else
        i
      end
    }.flatten.map{|j|
      if (defined? j.exclude_begin?)
        j
      else
        RangeExtd(j)
      end
    }
  end

  # Get the instance variable (Hash) @infinities
  #
  # @param in_infs [Hash] infinities template with keys: :positive, :negative
  # @param arin [Array<Range, RangeExtd, Rangeary>]
  # @param strict: [Boolean] if true (Def: false), set only when the value satisfies RangeExtd::Infinity.infinite?
  # @param leave_existing: [Boolean] if true (Def: false), the existing non-nil values are not updated.
  # @return [Array] Best guess for @infinities
  def _best_guessed_infinities(in_infs, arin, strict: false, leave_existing: false)
    reths = {}.merge in_infs
    # Making the best effort to guess.
    ar_rae = [RangeExtd::Infinity::NEGATIVE, RangeExtd::Infinity::POSITIVE]
    ar_num = [-Float::INFINITY, Float::INFINITY]
    [arin].flatten.each do |ea|
      hs = { negative: ea.begin, positive: ea.end }
      reths.each_key do |ek|
        errmsg = "Contradictory reference value #{hs[ek].inspect} for infinity (Existent: #{reths[ek].inspect}) is found, but ignored."
        next if !hs[ek]
        case reths[ek]
        when nil
          reths[ek] =  _guessed_infinity(ek, hs[ek], strict: strict)
          next
        when *ar_rae
          reths[ek] = (_guessed_infinity(ek, hs[ek], strict: strict) || reths[ek]) if !leave_existing   # Update whatever.
          next
        when *ar_num
          # Once Float::INFINITY is set, it will unchange.
          next if ar_num.include? _guessed_infinity(ek, hs[ek])  # Consistent
          next if ar_rae.include? _guessed_infinity(ek, hs[ek])  # Inconsistent, but ignore (Prev: Range::Inf, Given: Float::INF)
          warn errmsg if !$VERBOSE.nil?
          next
        else
          # Once User's value is set, it will unchange.
          next if ar_rae.include? _guessed_infinity(ek, hs[ek])  # Inconsistent, but ignore (Prev: User, Given: Infinity)
          warn errmsg if !$VERBOSE.nil?
          next
        end
      end
    end
    reths
  end
  private :_best_guessed_infinities

  # Instance method version
  #
  # where @infinities are taken into account.
  #
  # @param ran [Range, RangeExtd]
  # @return [Object]
  def _comparable_end(ran)
    if ran.class.method_defined?(:is_none?) && ran.is_none?
      nil
    elsif ran.end.nil? && ran.begin.nil?
      nil
    elsif ! ran.end.nil?
      ran.end  # Before Ruby 2.6, this is always the case.
    else
      infinities[:positive] || RangeExtd::Infinity::POSITIVE
    end
  end
  private :_comparable_end

  # Same as {#_comparable_end} but for begin
  #
  # Beyond the current Ruby (2.6).
  # Needed for {#negation}.
  #
  # @param rbeg [Object]
  # @param rend [Object]
  # @return [Object]
  def _comparable_begin(rbeg, rend)
    if rend.nil? && rbeg.nil?
      nil
    elsif ! rbeg.nil?
      rbeg
    elsif rend.class.method_defined? :to_int
      -Float::INFINITY
    else
      RangeExtd::Infinity::NEGATIVE
    end
  end
  private :_comparable_begin


  # Get the instance variable (Hash) @infinities
  #
  # @param arin [Array<Range, RangeExtd, Rangeary>]
  # @param inherited_infs [Array<Hash<Infinity, nil>>] Inherited infinities from the input Rangeary-s
  # @param guess_strict [Boolean] if true, make only strict guess for infinities, namely, unless the existing elements contain "infinity"-type objects, leave it nil; it is passed to {#_best_guessed_infinities}(strict: false)
  # @option **opts [Object] :positive Object for positive infinity. In default {Float::INFINITY} for Numeric Comparable or else {RangeExtd::Infinity::POSITIVE}.
  # @option **opts [Object] :negative Object for negative infinity. In default -{Float::INFINITY} for Numeric Comparable or else {RangeExtd::Infinity::NEGATIVE}.
  # @return [Array] guessed @infinities
  def _get_infinities(arin, inherited_infs: [], guess_strict: false, **opts)
    # Explicitly specified?
    reths = _get_infinities_from_opts(opts)
    if reths
      _validate_opts_infinities(arin, infs: reths)
      # return reths if (reths.all?{ |i| i[1] })
    end
    leave_existing = !!reths

    hs = {}.merge(@infinities)
    hs.each_key {|k| hs[k] = nil }  # :positive, :negative => nil
    reths ||= hs

    # Read @infinities from Rangeary-s in arin, if exits
    reths = self.class.send(:_validate_select_infinities, inherited_infs, reths)

    return reths if (reths.all?{ |i| i[1] })
    leave_existing ||= reths.any?{ |i| i[1] }

    _best_guessed_infinities(reths, arin, strict: guess_strict, leave_existing: leave_existing)
  end
  private :_get_infinities

  # Returns @infinities from the options.
  #
  # If not specified actually, returns nil.
  #
  # @param opts [Hash]
  # @return [Hash, nil]
  def _get_infinities_from_opts(opts)
    hsret = {}
    if opts.keys.include?(:positive) || opts.keys.include?(:negative)
      if opts[:positive]
        hsret[:positive] = opts[:positive]
        opts[:negative] = -Float::INFINITY if is_num_type? opts[:positive]
        hsret[:negative] ||= opts[:negative]
        return hsret
      elsif opts[:negative]
        hsret[:negative] = opts[:negative]
        opts[:positive] = Float::INFINITY if is_num_type? opts[:negative]
        return hsret
      end
    end
    return nil
  end
  private :_get_infinities_from_opts

  # Returns the best guess Infinity from a given value (like 5.2 or "a")
  #
  # @param key [Symbol] either :positive or :negative
  # @param val [Object] from which Infinity is guessed.
  # @param strict [Boolean] if strict, unless the value is a kind of Infinite, return nil
  # @return [Hash<Array>] keys (:positive and :negative) Array#size may not agree between them.
  def _guessed_infinity(key, val, strict: false)
    return nil if !val
    return(RangeExtd::Infinity.infinite?(val) ? val : nil) if strict
    return(((key == :positive) ? 1 : -1) * Float::INFINITY) if is_num_type? val
    ((key == :positive) ? RangeExtd::Infinity::POSITIVE : RangeExtd::Infinity::NEGATIVE)
  end
  private :_guessed_infinity


  # Instance method version
  def is_num_type?(obj)
    self.class.send(__method__, obj)
  end
  private :is_num_type?

  # Normalize a Range, which is about to be used for new {Rangeary}
  #
  # @param rbeg [Object]
  # @param rend [Object]
  # @return [Range, RangeExtd::NONE]
  def normalized_range_for_negation(rbeg, rend)
    begin
      ret = rbeg..rend  # Range
    rescue RangeError
      # the begin must be nil, after being converted from an Endless Range as in Ruby 2.6
      ret = (_comparable_begin(rbeg, rend)..rend)  # Range
    # rescue ArgumentError
    ## NOTE: If this happens, something is gone wrong!!
    end
    return RangeExtd::NONE if same_infinities?(ret.begin, ret.end)
    ret
  end
  private :normalized_range_for_negation

  # True if both are infinities and in the same parity
  #
  # @param c1 [Object]
  # @param c2 [Object]
  def same_infinities?(c1, c2)
    arin = [c1, c2]
    arin.all?{ |ec| RangeExtd::Infinity.infinite?(ec) } &&
      (arin.all?(&:positive?) || arin.all?(&:negative?))
  end
  private :same_infinities?

  # Validate @infinities from the options.
  #
  # If fails, raise Exception.
  #
  # @param arin [Array<Range, RangeExtd, Rangeary>]
  # @param infs [Hash]
  # @return [void]
  # @raise [ArgumentError]
  def _validate_opts_infinities(arin, infs: @infinities)
    infs.each_pair do |ek, my_inf|
      next if !my_inf #|| RangeExtd::Infinity.infinite?(my_inf)
      arin.flatten.each do |er|
        [er.begin, er.end].each do |ev|
          next if !ev
          next if (is_num_type?(ev) && is_num_type?(my_inf))
          begin
            case my_inf <=> ev
            when -1, 0, 1
              next
            else # nil
            end
          rescue # NoMethodError
          end
          msg = "invalid parameter for :#{ek} => (#{my_inf.inspect}), incompatible with the range with Range=(#{er.inspect})."
          raise ArgumentError, msg
        end
      end
    end
  end
  private :_validate_opts_infinities


  # Legacy routine to set the instance variable (Hash) @infinities
  #
  # Maybe more complete?
  #
  # @param [Array]
  # @param [Hash]
  # @param [Hash]
  # @param [Hash]
  # @return [void]
  def set_infinities_old(arRange, hsInheritedObj, hsInheritedAry, hsInheritedClass, **opts)

    ### The following is required in the caller before calling this routine
    ## inarall is the arguement received by initialize()
    #
    #
    # hsInheritedObj   = {:negative =>nil, :positive =>nil}
    # hsInheritedAry   = {:negative => [], :positive => []}
    # hsInheritedClass = {:negative => [], :positive => []}
    # inarall = inarall.map{|i|
    #   if defined?(i.first_element) && defined?(i.infinities)
    #     begin
    #       [:negative, :positive].each do |nega_posi|
    #         hsInheritedAry[nega_posi].push(  i.infinities[nega_posi])
    #         hsInheritedClass[nega_posi].push(i.infinities[nega_posi].class)
    #       end
    #     rescue
    #       warn "warning: Rangeary#infinities looks wrong in the input (#{i})."
    #     end
    #     i.to_a
    #   else
    #     i
    #   end
    # }.flatten.map{|j|
    #   if (defined? j.exclude_begin?)
    #     j
    #   else
    #     begin
    #       RangeExtd.new(j)
    #     rescue ArgumentError, RangeError  # Just to change the error message.
    #       raise ArgumentError, "invalid parameter for RangeExtd, hence for Rangeary (#{j.inspect})."
    #     end
    #   end
    # }

    # Check inherited objects if there is any, namely if the argument includes any RangeAry object.
    # Priority: Float > Others > RangeExtd::Infinity
    if hsInheritedAry[:negative].size > 0
      [:negative, :positive].each do |es| 
        iFloat = hsInheritedClass[es].find_index(Float)
        if iFloat.nil?
          iElse = hsInheritedClass[es].find_index{|i| (i != RangeExtd::Infinity) && (i != Float)}
          if iElse.nil?
            iRangeInf = hsInheritedClass[es].find_index(RangeExtd::Infinity)
            if iRangeInf.nil?
              raise "Rangeary#infinities is not set in the input."	# Should not happen, as Rangeary#infinities must be set always.
            else
              hsInheritedObj[es] = hsInheritedAry[es][iRangeInf]
            end
          else
            hsInheritedObj[es] = hsInheritedAry[es][iElse]
          end
        else
          hsInheritedObj[es] = hsInheritedAry[es][iFloat]
        end	# if iFloat.nil?
      end	# [:negative, :positive].each do |es| 
    end		# if hsInheritedAry.size > 0

    # Determines what the infinities are: either Float::INFINITY or RangeExtd::Infinity::(POSI|NEGA)TIVE
    hsFlag = { :found => {:negative => false, :positive => false} }
    hsCand = {
      :negative => arRange[0].begin,
      :positive => _comparable_end(arRange[-1]),
      # :pos_orig => arRange[-1].end,  # may be nil in Ruby-2.6
    }
    infDef = { :negative => RangeExtd::Infinity::NEGATIVE, :positive => RangeExtd::Infinity::POSITIVE }
    @infinities={ :negative => nil, :positive => nil }
    [:negative, :positive].each do |es|
      if (infDef[es] == hsCand[es])	# Can be Float or whatever.
        @infinities[es] = hsCand[es]	# highest priority
        hsFlag[:found][:negative] = true
      else
        strtmp = ""
        [opts[es], hsInheritedObj[es]].each do |opts_or_inherited|
          @infinities[es] ||= opts_or_inherited		# uses ots[:****tive] or hsInheritedObj[:****tive] if not set.
          # Now, checking the compatibility of the infinity value specified (or inherited) with the given range.
          if (! opts_or_inherited.nil?) && (opts_or_inherited == @infinities[es]) && (! arRange[0].is_none?)
            begin
              _ = 0 * (opts_or_inherited <=> hsCand[es])
            rescue TypeError
              raise ArgumentError, "invalid #{strtmp}parameter for :#{es} => (#{opts_or_inherited.inspect}), incompatible with the range with begin=(#{hsCand[es].inspect})."
            end
          end
          strtmp = "inherited "
        end	# [opts[es], hsInheritedObj[es]].each do |opts_or_inherited|
      end	# if (infDef[es] == hsCand[es])	# else
    end	# [:negative, :positive].each do |es| 

    if ! (@infinities[:negative] && @infinities[:positive])
      # Either or both @infinities[:negative, :positive] is not set, yet.
      # Need to set it now.  The possibilities are,
      #  (1) arRange[0].null? && no opts/inheritance given.
      #  (2) one of them is given by either arRange or opts or inheritance, but not the other.
      #  (3) neither of them is given by arRange nor opts nor inheritance.
      if arRange[0].null?
        [:negative, :positive].each do |es| 
          @infinities[es] ||= infDef[es]
        end
      else
        # There must be a non-infinity object - we will find it out.
        if hsFlag[:found][:negative]
          obj2refer = _comparable_end(arRange[0])  # For Ruby-2.6 Endless Range
          # obj2refer = arRange[0].end
        else
          obj2refer = arRange[-1].begin
        end

        # Now, if Numeric === obj2refer, Float::INFINITY is the default.
        begin
          _dummy = (1.0 < obj2refer)
        rescue ArgumentError
          # Not Numeric, hence the current infDef is used as it is.
        else
          # Numeric
          infDef = { :negative => -Float::INFINITY, :positive => Float::INFINITY }
        end
        [:negative, :positive].each do |es| 
          @infinities[es] ||= infDef[es]	# uses default infinity if not set.
        end
      end	# if arRange[0].null?
    end		# if ! (@infinities[:negative] && @infinities[:positive])
  end
  private :set_infinities_old

  # Called from {Rangeary#initialize}.
  #
  # Process the array of RangeExtd and return the new one, in which
  # overlapped ranges are merged accordingly.
  #
  # If there is no non-"empty" range, one of them will be left.
  # As a priority, an empty range with a definite class is left,
  # but if there is none, RangeExtd::NONE will be left.
  #
  # Note that (Inf..Inf) or (-Inf..-Inf) is replaced with +RangeExtd::NONE+,
  # which then will be truncated.
  #
  # @param inAr [Array<RangeExtd,Range>]
  # @return [Array]
  def _merge_overlaps(inAr)
  #def self.compact(inAr)

    ### Cases
    #[st means status.]
    #(0) (prev[0]) and (prev[0].status) unchanged.
    #(1) if (now[-1]<  prev[-1]), do nothing. [Totally inclusive]
    #   I---* => I---*
    #   *--*
    #(2) if (now[-1]== prev[-1])
    # (2-1) AND if (now[-1].st? || prev[-1].st?), prev[-1].st=T [Nearly inclusive]
    #   I--O => I--I
    #   I--I
    # (2-2) ELSE do nothing. [Totally inclusive]
    #(3) ELSE  [namely, if (now[-1] > prev[-1])]
    # (3-1) if (now[0] >  prev[-1]), append. [Totally exclusive]
    #   *--*    => *--* *--*
    #       *--*
    # (3-2) if (now[0] == prev[-1])
    #  (3-2-1) (!now[0].st? && !prev[-1].st?), append. [Totally exclusive]
    #   *--O    => *--O--*
    #      O--*
    #  (3-2-2) ELSE [namely, now[1].st? || prev[-1].st?], connect. (prev[0],now[-1])
    #   *--O    => *-----*
    #      I--*
    #   *--I    => *-----*
    #      O--*
    #   *--I    => *-----*
    #      I--*
    # (3-3) ELSE  [namely, if (now[0] <  prev[-1])], connect. (prev[0],now[-1])
    #   *--*  => *---*
    #    *--*
    #

    inRanges = _replace_inf_inf(inAr)  # Replace meaningless inf..inf etc.
    inRanges = self.class.sort_ranges(inRanges).map{|i| (RangeExtd === i) ? i : RangeExtd(i) }	# => Rangeary.sort_ranges(ar)

    if inRanges.size < 1
      return inRanges
    end

    newRanges = [inRanges[0]]

    inRanges[1..-1].each do |eachr|
      prev = newRanges[-1]

      # To deal with Ruby-2.6 Endless Range like (5..) (=(5..nil))
      # *.end is guaranteed not to be false.
      eachr_end = _comparable_end(eachr)
      prev_end  = _comparable_end(prev)

      case eachr_end <=> prev_end
      when -1	# aka, eachr_end < prev_end
        # Do nothing [Totally inclusive]
      when 0
        if (!eachr.exclude_end?) && prev.exclude_end?
          # Change the status (:exclude_end => false) for prev
          newRanges[-1] = RangeExtd.new(prev.begin, prev.end, :exclude_begin => prev.exclude_begin?, :exclude_end => false)
        else
          # Do nothing [Totally inclusive]
        end
      when 1	# aka, eachr_end > prev_end
        case eachr.begin <=> prev_end
        when -1	# Connect by combining
          newRanges[-1] = RangeExtd.new(prev.begin, eachr.end, :exclude_begin => prev.exclude_begin?, :exclude_end => eachr.exclude_end?)
        when 0
          if (eachr.exclude_begin?) && (prev.exclude_end?)
            newRanges.push(eachr)	# [Totally exclude]
          else	# Connect by combining
            newRanges[-1] = RangeExtd.new(prev.begin, eachr.end, :exclude_begin => prev.exclude_begin?, :exclude_end => eachr.exclude_end?)
          end
        when 1
          newRanges.push(eachr)	# [Totally exclude]
        when nil
          newRanges.push(eachr)	# must be RangeExtd::NONE (or user-defined equivalent)
        else
          raise
        end	# case eachr.begin <=> prev_end
      when nil	# aka, eachr_end > prev_end
        newRanges.push(eachr)	# must be RangeExtd::NONE (or user-defined equivalent)
      else
        raise
      end	# case eachr_end <=> prev_end 

    end		# inRanges[1..-1].each do |eachr|


    ## Sort out empty Ranges in the array.
    # If there is at least one non-empty range, delete all empty ranges.
    # If not, leave one of them, preferably not RangeExtd::NONE,
    # unless there is no choice.
    hsFlag = {
      :empty? => true,
      :klass  => nil,
      :found? => false,
    }

    # Search for non-empty range.
    newRanges.each do |er|
      if er.empty?
        if hsFlag[:klass].nil?
          obj = er.begin()
          if obj.nil?
            ## Do nothing
          else
            hsFlag[:klass] = obj.class
          end
        end
      else
        hsFlag[:empty?] = false
        break
      end
    end

    hsFlag[:found?] = false	# Redundant, but for the sake of readability
    if hsFlag[:empty?]
      # It is all empty, hence delete all but one.
      hsFlag[:klass] = NilClass if hsFlag[:klass].nil?
      newRanges.delete_if do |er|
        if hsFlag[:found?]
          true
        elsif er.begin().class == hsFlag[:klass]
          hsFlag[:found?] = true
          false
        else
          true
        end
      end
    else
      # Deletes all the empty ones.
      newRanges.delete_if { |er| er.empty? }
    end

    newRanges
  end	# def _merge_overlaps(inAr)

  private :_merge_overlaps

  # Replaces the invalid inf..inf Range with NONE
  #
  # @param arin [Array<Range, RangeExtd>]
  # @return [Array]
  def _replace_inf_inf(arin)
    arin.map{ |er|
      raise 'contact the code developer' if !defined? er.exclude_end?  # Sanity check.
      arran = [er.begin, er.end] # to_a raises RangeError for Ruby 2.6 Endless Range
      if (( arran.all?{ |ea| RangeExtd::Infinity.infinite?(ea) } &&
           (arran.all?(&:positive?) || arran.all?(&:negative?)) ) ||
          (arran.all?(&:nil?)))
        RangeExtd::NONE
      else
        er
      end
    }
  end
  private :_replace_inf_inf

  ####################
  # private_class_method
  ####################

  # Sort infinities obtained from inherited objects and returns the best one
  #
  # RangeExtd::Infinity is ignored.  Float::INFINITY has the lowest priority.
  #
  # @param *ar_infs [Array] of Infinities Hash (inherited)
  # @return [Hash] each key (:(posi|nega)tive) contains a single value (potentially null) for infinity.
  def self._best_inherited_infinities(*ar_infs)
    hsar = _sort_inherited_infinities_all( _get_cand_infinities(ar_infs.flatten) )
    hsar.map{ |k, ev|
      [k, ev[0]]
    }.to_h  # Ruby 2.1 or later
  end
  private_class_method :_best_inherited_infinities

  #== Logical conjunction of two RangeExtd
  #
  # To assure this logical conjunction meaningful,
  # the objects that consist of RangeExtd objects have to be
  # monotonic (increase), namely, for any potential element,
  # x_n and x_m, within the given range,
  #    (x_n <=> x_m) == 1  if  (n > m),
  # have to be true.  In other words, the derivative must be always
  # non-negative.
  #
  # For example, (?a..?d) and (?x..?z) satisfies this condition.
  # However, ('d'..'gg') does not, as follows.
  #   rd = RangeExtd('d'..'gg')
  #   rf = RangeExtd('f'..'h')
  #   Rangeary.conjunctionRangeExtd(rd, rf)  # => ('f'..'gg')
  #
  # @note If you give a built-in Range object(s) for the arguments,
  #   make sure they are valid, that is, Range#valid? returns true.
  #
  #=== Algorithm
  #
  #[st means status. - true if excl; Estimate (Init(in|ex),Fini(in|ex))]
  #[b4 <= af] (sort!)
  #
  #(1) Init = af[0],  Init.st=af[0].st
  #   If (b4[0]==af[0]),   then Init.st = ( b4[0].ex || af[0].ex)
  #(2) Fini = [b4[-1], af[-1]].min, which belongs to (b4|af)
  #   If (b4[-1]==af[-1]), then Fini.st = (b4[-1].ex || af[-1].ex), otherwise nil for now.
  #(3) if (Init > Fini) => none.
  #   *---*  => ....
  #        *--*     ....
  #(4) if (Init == FiniMax), then Fini.st=b4[-1].st
  # (4-1) if (Init.in&&Fini.in),  => Single-Number-Range(InitCand(in))
  #  *--I  => ...I
  #     I--*     I...
  # (4-2) else, => none
  #(5) if (Init < FiniMax)
  # (5-1) if Fini belongs to b4, Fini.st=b4[-1].st
  #  *---* => .*--* 
  #   *---*
  # (5-2) if Fini belongs to af, Fini.st=af[-1].st
  #  *---* => .*-* 
  #   *-*
  # (5-3) if Fini belongs to both, Fini.st is defined already.
  #  *---* => .*--* 
  #   *--*
  # 
  # @param r1 [RangeExtd] Can be Range
  # @param r2 [RangeExtd] Can be Range
  # @return [RangeExtd]
  # 
  def self.conjunctionRangeExtd(r1, r2)

    [r1, r2].each do |er|
      return er if er.is_none?
    end

    r = *( sort_ranges([RangeExtd(r1), RangeExtd(r2)]) )	# => Rangeary.sort_ranges

    ## Note: the end product will be (cBeg(:stBeg), cEnd(:stEnd))
    #    where :stBeg and :stEnd mean exclude_(begin|end)?

    # Set the candidate begin value.
    cBeg  = r[1].begin
    if r[0].begin == r[1].begin
      stBeg = (r[1].exclude_begin? || r[0].exclude_begin?)
    else
      stBeg =  r[1].exclude_begin?
    end

    # Set the candidate end value.  (comparable_end() for Ruby-2.6 Endless Range)
    # Note: this comparison ignores @infinities even if set,
    #   because @infinities may not be defined in the arguments!
    #   Anyway, nothing should be larger than the upper limit
    #   and so this should be fine.
    if comparable_end(r[0]) == comparable_end(r[1])
      cEndOrig = r[1].end
      cEnd     = comparable_end(r[1])
      stEnd = (r[0].exclude_end? || r[1].exclude_end?)
    else
      a = [[comparable_end(r[0]), 0, r[0].end], [comparable_end(r[1]), 1, r[1].end]].min
      cEnd  = a[0]
      cEndIndex = a[1]	# r[cEndIndex] == RangeExtd obj that gives the end of the resultant range.
      cEndOrig = a[2]
      stEnd = nil
    end

    case cBeg <=> cEnd
    when 1	# cBeg > cEnd
      RangeExtd::NONE

    when 0	# cBeg == cEnd
      stEnd = r[0].exclude_end? 
      if (!stBeg) && (!stEnd)
        RangeExtd(cBeg..cBeg)	# Point range
      else
        RangeExtd::NONE
      end

    when -1	# cBeg < cEnd
      # Now, the range must be (cBeg, cEnd).  May need adjustment of the exclude status.
      if stEnd.nil?
        stEnd = r[cEndIndex].exclude_end?
      # else
      # # Already defined.
      end	# if stEnd.nil?

      RangeExtd(cBeg, cEndOrig, :exclude_begin => stBeg, :exclude_end => stEnd)
    else
      raise
    end		# case cBeg <=> cEnd

  end	# def self.conjunctionRangeExtd(r1, r2)
  private_class_method :conjunctionRangeExtd

  # Returns the candidate @infinities from the input Rangeary
  #
  # If there is any Rangeary in the input.
  #
  # Example return:
  #   { :positive => [nil,  Float::INFINITY],
  #     :negative => [nil, -Float::INFINITY] }
  #
  # Note the standard @infinities is a Hash, but NOT a Hash of Array.
  # This method returns the candidates.
  #
  # @param arin [Array<Hash<Infinity, nil>>] Inherited infinities from the input Rangeary-s
  # @return [Hash<Array>] keys (:positive and :negative) Array#size may not agree between them.
  def self._get_cand_infinities(arin)
    hsret = { positive: [], negative: [] }
    arin.each do |ec|
      hsret.each_key do |k|
        hsret[k] << ec[k]
      end
    end
    hsret
  end
  private_class_method :_get_cand_infinities

  # True if object is a type of Rangeary
  def self.is_rangeary_type?(obj)
    obj.respond_to?(:infinities) && obj.class.method_defined?(:first_element)
  end
  private_class_method :is_rangeary_type?

  # True if object is a type of Numeric and comparable
  def self.is_num_type?(obj)
    Numeric === obj && obj.respond_to?(:between?) && obj.class.method_defined?(:<)
  end
  private_class_method :is_num_type?

  # Sort infinities obtained from inherited objects
  #
  # RangeExtd::Infinity is ignored.  Float::INFINITY has the lowest priority.
  #
  # @param hs_inherit [Hash<Array>] :positive => Array, etc (From Rangeary-s in the main Array given)
  # @return [Hash<Array>] each key (:(posi|nega)tive) contains the sorted candidate Array.
  def self._sort_inherited_infinities_all(hs_inherit)
    hs_inherit.map do |ek, ev|
      [ek, _sort_inherited_infinities_each(ev, key=ek)]
    end.to_h  # Ruby 2.1 or later
  end
  private_class_method :_sort_inherited_infinities_all

  # Sort infinities obtained from inherited objects
  #
  # RangeExtd::Infinity is ignored.  Float::INFINITY has the lowest priority.
  #
  # @param ar_infs [Array] of Infinities (inherited)
  # @param key [Symbol] :positive or :negative
  # @return [Array]
  def self._sort_inherited_infinities_each(ar_infs, key=:positive)
    ar_infs.map {|j|
      (RangeExtd::Infinity === j) ? nil : j
    }.compact.sort{|a,b|
      if is_num_type?(a) && RangeExtd::Infinity.infinite?(a)
        -1
      elsif is_num_type?(b) && RangeExtd::Infinity.infinite?(b)
        1
      else
        begin
          (key == :positive) ? (a<=>b) : (b<=>a)
        rescue
          0
        end
      end
    }
  end
  private_class_method :_sort_inherited_infinities_each

  # Validate the infinities by Options and inherited and select the best
  #
  # If Option is specified, that has the priority.
  # Among the inherited, the youngest non-nil one has the highest priority.
  #
  # If there are any inconsistencies, issue a warning message, if $VERBOSE==true.
  #
  # Note this method does not look at the contents of the Range Array given.
  #
  # @param ar_inherit [Array<Hash<Infinity, nil>>] Inherited infinities from the input Rangeary-s
  # @param hs_opts [Hash, nil] :positive, :negative, specified by the option to {Rangeary.initialize}
  # @return [Hash] keys (:positive and :negative) with a single value for each
  def self._validate_select_infinities(ar_inherit, hs_opts=nil)
    hs_inherit = _get_cand_infinities(ar_inherit)
    if !hs_opts
      hs_opts = hs_inherit.map{ |ek, ev|
        [hs_inherit[ek], nil]
      }.to_h  # Ruby 2.1 or later
    end
    # e.g., hs_inherit[:positive] == [ "z"(From-Option), nil(Inherited), "y"(Inherited), INFINITY(inherited) ]

    # Selection
    hsret = _sort_inherited_infinities_all( hs_inherit ).map{ |ek, ev|
      [ek, ([hs_opts[ek]]+ev).compact[0]]
    }.to_h  # Ruby 2.1 or later

    # Validation (for warning, issued when $VERBOSE==true)
    hs_inherit.each_pair do |ek, ev|
      ev_uniq = ([hs_opts[ek]]+ev).compact.uniq
      msg = "Inconsistent %s infinities are found: %s (=> %s is used)"%[ek, ev_uniq.inspect, hsret[ek]]
      #warn msg if $VERBOSE && (ev_uniq.size > 2) && (!ev_uniq.all?{ |c| RangeExtd::Infinity.infinite?(c) })
      warn msg if (ev_uniq.size > 1) && (!ev_uniq.all?{ |c| RangeExtd::Infinity.infinite?(c) })
    end

    hsret
  end
  private_class_method :_validate_select_infinities

end # class Rangeary < Array

# Overwrites its equal operator
class Array
  alias_method :equals_before_rangeary, :== if ! self.method_defined?(:equals_before_rangeary)

  # Updated Array#==
  #
  # This returns true even if the standard #{==} is false, if both are *practically* empty,
  # that is, if +#empty_element?+ and/or +empty?+ are true for both, they are equal.
  # Also, this equates the "end" of Endless Range (Ruby 2.6) with Float::INFINITY or
  # RangeExtd::Infinity::POSITIVE.
  # Note by definition, it would appear at the last element in Rangeary only,
  # if it does.
  #
  # @param other [Object]
  def ==(other)
    return true  if equals_before_rangeary other
    return false if !other.class.method_defined?(:to_ary)
    return false if !self.class.method_defined?(:empty_element?) && !other.class.method_defined?(:empty_element?)

    # It was false.  Is it?
    # eg., (Rangeary[RangeExtd::NONE] == []) is true,
    # because Rangeary[] with zero components does not exist!
    # Now either other or self is guranteed to be Rangeary.
    self_empt  = (respond_to?(:empty_element?) ? empty_element? : empty?)
    other_empt = (other.respond_to?(:empty_element?) ? other.empty_element? : other.empty?)
    return true  if self_empt && other_empt
    return false if self_empt ^  other_empt
    # return false if size != other.size  # evaluated at the beginning.
    return false if size >= 2 && self[0..-2] != other[0..-2]
    return false if !self[-1].respond_to?(:exclude_end?) || !other[-1].respond_to?(:exclude_end?)

    # Now, both are guaranteed to have the same number of non-zero elements,
    # all their elements except for the last one are equal,
    # and their last elements are Range-type instances.
    # Yet, the standard "equal" operator has failed.
    # Only the potential they may be equal is their last elements differ
    # between Endless Range (Ruby 2.6) and Infinity.
    c_self  = Rangeary.comparable_end self[-1]
    c_other = Rangeary.comparable_end other[-1]
    return false if c_self != c_other

    if !c_self.class.method_defined?(:infinite?) && !c_self.class.method_defined?(:infinity?)
      # :infinite? for Float::INFINITY, :infinity? is defined in RangeExtd::Infinity
      warn "sanity check failed. c_self should be infinite (#{c_self.inspect}). The result is not guranteed. Contact the code developer."
    end

    # The end of their last elements are both positive Infinity.
    # How about "begin"?
    self_flag  = (self[-1].class.method_defined?(:exclude_begin?)  ? self[-1].exclude_begin?  : false)
    other_flag = (other[-1].class.method_defined?(:exclude_begin?) ? other[-1].exclude_begin? : false)
    (self[-1].begin == other[-1].begin) && (self_flag == other_flag)
  end
end  # class Array

