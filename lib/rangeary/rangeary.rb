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
# {https://rubygems.org/gems/range_extd}
#
class Rangeary < Array
  undef_method :*, :+, :length, :reverse

  # Hash with the keys of :negative and :positive
  attr_reader :infinities

  #
  # Arbitrary (positive) number of arguments can be given.
  # (r1, [r2, ...])
  #
  # It is possible to supply the user-defined infinity objects for both or either
  # positive and negative infinity.  If one (or more) of the arguments is
  # a {Rangeary} object, their infinity values are inherited, unless explicitly
  # specified in the optional arguments.
  #
  # @param inarall [Object] An arbitrary number of either {Rangeary}, {RangeExtd} or {Range} objects (or its subclasses).
  # @option **opts [Object] :positive Object for positive infinity. In default {Float::INFINITY} for Numeric Real or else {RangeExtd::Infinity::POSITIVE}.
  # @option **opts [Object] :negative Object for negative infinity. In default -{Float::INFINITY} for Numeric Real or else {RangeExtd::Infinity::NEGATIVE}.
  def initialize(*inarall, **opts)

    if inarall.size < 1
      raise ArgumentError, "wrong number of arguments (#{inarall.size} for 1 or more)."
    end

    hsInheritedObj   = {:negative =>nil, :positive =>nil}
    hsInheritedAry   = {:negative => [], :positive => []}
    hsInheritedClass = {:negative => [], :positive => []}
    inarall = inarall.map{|i|
      if defined?(i.first_element) && defined?(i.infinities)
        begin
          [:negative, :positive].each do |nega_posi|
            hsInheritedAry[nega_posi].push(  i.infinities[nega_posi])
            hsInheritedClass[nega_posi].push(i.infinities[nega_posi].class)
          end
        rescue
          warn "warning: Rangeary#infinities looks wrong in the input (#{i})."
        end
        i.to_a
      else
        i
      end
    }.flatten.map{|j|
      if (defined? j.exclude_begin?)
        j
      else
        begin
          RangeExtd.new(j)
        rescue ArgumentError	# Just to change the error message.
          raise ArgumentError, "invalid parameter for RangeExtd, hence for Rangeary (#{j.inspect})."
        end
      end
    }

    # _merge_overlaps
    begin
      arRange = _merge_overlaps( convert2range(inarall) )
    rescue => err
      # Trap just to change the type of the exception.
      raise ArgumentError, err.message, err.backtrace
    end

    if arRange.empty?
      raise ArgumentError, 'no significant argument given for Rangeary.'
    end

    ## Setting @infinities[:negative, :positive]

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

    hsFlag = { :found => {:negative => false, :positive => false} }
    hsCand = { :negative => arRange[0].begin, :positive => arRange[-1].end }
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
          obj2refer = arRange[0].end
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

    super(arRange)
    self.freeze
  end	# def initialize(*inarall, **opts)

  alias_method :triple_equals_orig, :=== if ! self.method_defined?(:triple_equals_orig)

  # If self covers the entire range?
  #
  def all?
    rfirst = self[0]
    ((1 == size) &&
     (@infinities[:negative] == rfirst.begin) &&
     (@infinities[:positive] == rfirst.end) &&
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
  def flatten
    to_a.reduce([]){|a,b| a+=b.to_a}
  end	# def flatten


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


  # Negation.
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
    prevend = nil
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
          return Rangeary.new(@infinities[:negative]..@infinities[:positive])
        end
      end

      ea_b = eachr.begin
      if (RangeExtd::Infinity::NEGATIVE == ea_b)	# Including -Float::INFINITY and other general negative infinities (nb., [#==] is not commutative!).
        # The existing first range starts from the negative infinity, so skip this one.
      else
        if prevend.nil?
          # The returned first range starts from the negative infinity.
          arran.push( RangeExtd(@infinities[:negative]..eachr.begin, :exclude_end => (! eachr.exclude_begin?)) )
        else
          arran.push( RangeExtd(prevend, eachr.begin, :exclude_begin => (! prevst), :exclude_end => (! eachr.exclude_begin?)) )
        end
      end	# if (eachr.begin == -Float::INFINITY) || (RangeExtd::NONE == eachr.begin)
      prevend = eachr.end
      prevst  = eachr.exclude_end?
    end		# to_a.each do |eachr|

    if (RangeExtd::Infinity::POSITIVE == prevend)	# Including Float::INFINITY and other general positive infinities (nb., [#==] is not commutative!).
      ## Do nothing
    else
      arran.push( RangeExtd.new(prevend..@infinities[:positive], :exclude_begin => (! prevst)) )
    end

    Rangeary.new(arran, :positive => @infinities[:positive], :negative => @infinities[:negative])
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
    hsInherited = {}
    [:negative, :positive].each do |ei|
      hsInherited[ei] = [r1.infinities[ei], r2.infinities[ei]].map{|j|
        (RangeExtd::Infinity === j) ? nil : j
      }.compact.sort{|a,b|
        if a.class == Float
          -1
        elsif b.class == Float
          1
        else
          0
        end
      }[0]
    end

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
        break if ea2.end < ea1.begin	# Completely out of range
        if ea1.end < ea2.begin		# Completely out of range
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


  # Returns the array sorted, based on (1) the begin objects and their boundary state,
  # (2) then the end objects and their boundary state.
  # {RangeExtd::NONE} comes first, if included in the input array.
  #
  # @param ar [<Range, RangeExtd>] Arbitrary number.
  # @return [Array<Range, RangeExtd>]
  def self.sort_ranges(*ar)
    ar.flatten.sort{ |a,b|
      err_msg_ab = "invalid parameter (#{a.inspect} or #{b.inspect})."
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
          ret = a.end <=> b.end
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
            if a.end.nil? && b.end.nil?
              0
            elsif a.end.nil?
              -1
            elsif b.end.nil?
               1
            else
              raise(TypeError, err_msg_ab)
            end
          else	# case ret # a.end <=> b.end
            raise(TypeError, err_msg_ab)
          end	# case ret # a.end <=> b.end
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


  # Called from {Rangeary#initialize}.
  # Process the array of RangeExtd and return the new one, in which
  # overlapped ranges are merged accordingly.
  #
  # If there is no non-"empty" range, one of them will be left.
  # As a priority, an empty range with a definite class is left,
  # but if there is none, RangeExtd::NONE will be left.
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

    # inRanges = sort_arrange( inAr )
    inRanges = self.class.sort_ranges( inAr ).map{|i| (RangeExtd === i) ? i : RangeExtd(i) }	# => Rangeary.sort_ranges(ar)

    if inRanges.size < 1
      return inRanges
    end

    newRanges = [inRanges[0]]

    inRanges[1..-1].each do |eachr|
      prev = newRanges[-1]
      case eachr.end <=> prev.end 
      when -1	# aka, eachr.end < prev.end
        # Do nothing [Totally inclusive]
      when 0
        if (!eachr.exclude_end?) && prev.exclude_end?
          # Change the status (:exclude_end => false) for prev
          newRanges[-1] = RangeExtd.new(prev.begin, prev.end, :exclude_begin => prev.exclude_begin?, :exclude_end => false)
        else
          # Do nothing [Totally inclusive]
        end
      when 1	# aka, eachr.end > prev.end
        case eachr.begin <=> prev.end
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
        end	# case eachr.begin <=> prev.end
      when nil	# aka, eachr.end > prev.end
        newRanges.push(eachr)	# must be RangeExtd::NONE (or user-defined equivalent)
      else
        raise
      end	# case eachr.end <=> prev.end 

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


  ####################
  # private_class_method
  ####################

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
    # r = *( sort_arrange([RangeExtd(r1), RangeExtd(r2)]) )

    ## Note: the end product will be (cBeg(:stBeg), cEnd(:stEnd))
    #    where :stBeg and :stEnd mean exclude_(begin|end)?

    # Set the candidate begin value.
    cBeg  = r[1].begin
    if r[0].begin == r[1].begin
      stBeg = (r[1].exclude_begin? || r[0].exclude_begin?)
    else
      stBeg =  r[1].exclude_begin?
    end

    # Set the candidate end value.
    if r[0].end == r[1].end
      cEnd = r[1].end
      stEnd = (r[0].exclude_end? || r[1].exclude_end?)
    else
      a = [[r[0].end, 0], [r[1].end, 1]].min
      cEnd  = a[0]
      cEndIndex = a[1]	# r[cEndIndex] == RangeExtd obj that gives the end of the resultant range.
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

      RangeExtd(cBeg, cEnd, :exclude_begin => stBeg, :exclude_end => stEnd)
    else
      raise
    end		# case cBeg <=> cEnd

  end	# def self.conjunctionRangeExtd(r1, r2)

  private_class_method :conjunctionRangeExtd


  # Logical conjunction of two Rangeary
  #
  #===Algorithm
  #
  #  r1 == [p1, p2, p3, ...]	# Array of RangeExtd
  #  r2 == [q1, q2, q3, ...]
  #  rc =  []	# Initial value of Conjunction r1*r2
  # 1. q1 = r2.shift
  # 2. r1.delete_if(pi.end<q1.begin) -> (r1==[P1,P2(>q1),P3,...])
  # 3. make conjunction between RangeExtd P1*q1 => pq1
  # 4. rc+=Rangeary.new(pq1)
  # 5a. if q1.end<P1.end, will do P1<->q2 next, hence return to 1 (will do r2.shift).
  #        *--* =>      *--*  + nil
  #   *--* *--*    .... *--*
  #      *---*  =>    .O--*   +    -*
  #   *---* *--*   ..... *--*   ...-* ----
  # 5b. if q1.end==P1.end, r1.shift, return to 1 (will do r2.shift).
  #    *--* *-- =>  .... *--  +  ---* ---
  #   *---* *--*   ..... *--*   .---* ----  
  # 5c. if q1.end>P1.end, will do P2<->q1, hence r1.shift, return to 2.
  #   *---* *-- => ..... *--
  #      *----        .O---
  #
  # @return [Rangeary] 
  def self.conjunction_orig(r1, r2)

    r1 = Rangeary.new(r1) if ! defined? r1.first_element
    r2 = Rangeary.new(r2) if ! defined? r2.first_element

    # Getting inherited options (if Rangeary is given) for the later use.
    hsInherited = {}
    [:negative, :positive].each do |ei|
      hsInherited[ei] = [r1.infinities[ei], r2.infinities[ei]].map{|j|
        (RangeExtd::Infinity === j) ? nil : j
      }.compact.sort{|a,b|
        if a.class == Float
          -1
        elsif b.class == Float
          1
        else
          0
        end
      }[0]
    end

    # Initialisation
    a1 = r1.to_a
    a2 = r2.to_a
    rc = Rangeary.new( RangeExtd::NONE, hsInherited )

    if a1.empty? || a2.empty?
      return rc
    end

    # Main loop
    hsFlag = {:a2shift => true}
    while true
      if hsFlag[:a2shift]
        if a2.size > 0
          q1 = a2.shift
        else
          break
        end
      end
      index_valid = a1.find_index{|i| i.begin >= q1.begin }
      if (! index_valid.nil?) && index_valid > 1
        a1.shift(index_valid)
      end

      if a1.size < 1
        break
      end
      p1 = a1[0]

      # Core - Perform conjunction.
      pq1 = Rangeary.conjunctionRangeExtd(p1, q1)
      rc += Rangeary.new(pq1)

      case q1.end <=> p1.end
      when -1	# q1.end < p1.end
        hsFlag[:a2shift] = true
      when 0
        if a1.size > 0
          a1.shift
        else
          break
        end
        hsFlag[:a2shift] = true
      when 1	# q1.end > p1.end
        if a1.size > 0
          a1.shift
        else
          break
        end
        hsFlag[:a2shift] = false
      else
        raise
      end	# case q1.end <=> p1.end
    end		# while (a1.size > 0) || (a2.size > 0)

    rc
  end
  private_class_method :conjunction_orig


  # Subtract a Range from an Array of Range objects.
  def self.subtractRange(arRng, r2sub)

    ### Cases
    #[st means status.]
    #(1) if (now[0] < prev[0])
    # (1-1) if (now[-1] < prev[0]), do nothing. [Totally exclusive]
    #        *--* => *--* 
    #   *--*
    # (1-2) if (now[-1] == prev[0])
    #  (1-2-1) AND if (now[-1].in?)&&(prev[-1].in?), Change prev[0].ex?=T [Nearly exclusive]
    #      I--* => O--* 
    #   *--I
    #  (1-2-1) ELSE, do nothing. [Totally inclusive]
    #      O--* => *--* 
    #   *--I
    #      *--* => *--* 
    #   *--o
    # (1-3) if (now[-1] > prev[0])
    #  (1-3-1) if (now[-1] < prev[-1]), => (now[-1],Prev[-1]).
    #    *---* => ..*-* 
    #   *--*
    #  (1-3-2) if (now[-1] == prev[-1]), => (now[-1],Prev[-1]).
    #   (1-3-2-1) if (now[-1].ex?)&&(prev[-1].in?), (Prev[-1],Prev[-1])(Single_number)
    #    *---I => ....I 
    #   *----O
    #   (1-3-2-2) ELSE, none.
    #    *---O => ..... 
    #   *----*
    #    *---* => ..... 
    #   *----I
    #  (1-3-3) if (now[-1] > prev[-1]), none
    #    *---* => ..... 
    #   *-----*
    #(2) if (now[0] == prev[0])
    # (2A-1) if (now[0].ex?)&&(prev[0].in?), => (Prev[0],Prev[0])(Single_number) + Any?
    #   I--- => I???
    #   O---
    # (2A-2) ELSE, none for the first part.
    #   O--- => .???
    #   *---
    #   *--- => .???
    #   I---
    # (2B) Follow (1) to get the New Array.
    #(3) if (now[0] > prev[0])
    # (3-1) if (now[-1] < prev[-1]), 2 Rangeary (Prev[0],now[0]),(now[-1],prev[-1])
    #  *-----* => *-*.*-*
    #    *-*
    # (3-2) if (now[-1] == prev[-1])
    #  (3-2-1) if (now[-1].ex?)&&(prev[-1].in?), => (Prev[0],now[0]),(prev[-1],prev[-1])
    #  *----I => *-*...I
    #    *--O
    #  (3-2-2) ELSE, => (Prev[0],now[0])
    #  *----I => *-*....
    #    *--I
    #  *----O => *-*....
    #    *--*
    # (3-3) if (now[-1] > prev[-1])
    #  (3-3-1) if (now[0] < prev[-1]), => (Prev[0],now[0]),(prev[-1],prev[-1])
    #  *---*  => *-*....
    #    *---*
    #  (3-3-2) if (now[0] == prev[-1])
    #   (3-3-2-1) if (now[0].in?)&&(prev[0].in?), => (Prev[0],prev[-1].ex)
    #  *--I  => *--O
    #     I--*
    #   (3-3-2-2) ELSE, unchanged.
    #  *--*  => *--*
    #     O--*
    #  *--O  => *--O
    #     *--*
    #  (3-3-3) if (now[0] > prev[-1]), unchanged.
    #  *--*  => *--*
    #      *--*
    #

    sort_ranges(arRng).map{ |er|	# => Rangeary.sort_ranges
    # sort_arrange(arRng).map{ |er|
      if    er.end < r2sub.begin
        er
      elsif er.begin < r2sub.begin && er.end < r2sub.end
        (er.begin .. (r2sub.begin-1))
      elsif er.begin < r2sub.begin && r2sub.end < er.end
        [er.begin .. (r2sub.begin-1), 
         (r2sub.end+1) .. er.end ]
      elsif r2sub.begin <= er.begin && er.end <= r2sub.end
        nil
      elsif r2sub.begin <= er.begin && er.begin <= r2sub.end && r2sub.end < er.end
        (r2sub.end+1) .. er.end
      elsif r2sub.end < er.begin
        er
      else
        raise "This should not happen."
      end
    }.flatten.compact 
  end	# def self.subtractRange(arRng, r2sub)
  private_class_method :subtractRange


end


