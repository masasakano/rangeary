# -*- coding: utf-8 -*-

require 'range_extd/load_all'

## Load required files in this library.
req_files = %w(rangeary/util)
req_files.each do |req_file|
  begin
    require_relative req_file
  rescue LoadError
    require req_file
  end
end

if $DEBUG
  req_files.push 'range_extd/load_all'
  puts "DEBUG(#{File.basename(__FILE__)}): Library full paths:"
  req_files.each do |elibbase|
    ar = $LOADED_FEATURES.grep(/(^|\/)#{Regexp.quote(File.basename(elibbase))}(\.rb)?$/).uniq
    print elibbase+": " if ar.empty?; p ar
  end
end

def Rangeary(*rest, **hs)
  # Note: This form (Rangeary(...)) is NOT used inside this file for the sake of searchability.
  #  The original "Rangeary.new" is used throughout.
  Rangeary.new(*rest, **hs)
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
# The library package +range_extd+ is required.
# {https://rubygems.org/gems/range_extd}
#
# An arbitrary number of {Rangeary}, RangeExtd or Range objects (or its
# descendant classes) can be supplied for the constructor {#initialize}.
# Then, an operation of disjunction is performed to get the clean Array of
# RangeExtd, none of which overlaps with one another.
#
# Once it is constructed, {Rangeary} objects are immutable.  Any subsequent
# operation returns a new {Rangeary} object.  All but few methods of {Array}
# are inherited, though some methods, such as +push+ do not work because of
# immutability of {Rangeary} objects.
#
# == Description
#
# The infinities are vital in the logical operation of Rangeary.
# Without it, negation could not be definied, and other logical
# operations are also closely related to it; for example, *subtraction*
# is basically a combination of *negation* and *conjunction*.
#
# To determine what the positive and negative infinities for the given
# elements is not a trivial task. In default, +nil+ is used except for
# +Numerics+ (Integer, Rational, Float etc), for which +Float::INFINITY+
# is used. Note that the default used to be
# <tt>RangeExtd::Infinity::POSITIVE</tt> and  <tt>RangeExtd::Infinity::NEGATIVE</tt>
# defined in {RangeExtd}[http://rubygems.org/gems/range_extd]
# up to Rangeary Ver.1, where both beginless and endless Ranges were not
# been introduced or supported. Rangeary Ver.2 changes the specification
# to be in line with the latest Ruby Range.
#
# Alternatively, a user can specify their own infinities in
# initialization of {Rangeary} with options of +positive:+ and
# +negative:+. The boundaries at the opposite polarities usually should
# match unless they are comparable or either of them is +nil+.
#
# Otherwise, ArgumentError might be
# issued if they contradict the elements; for example, if a {Rangeary}
# instance consists of an array of Integer Ranges (RangeExtd) like (3..8),
# and yet if String "abc" is specified as an infinity, it *contradicts*
# with the elements in the sense they are not comparable.
#
# Here are examples of how infinities work with {Rangeary}. In the first
# example, infinities are implicitly contained in the specified Range.
# Then, the infinities are internally preserved throughout operations.
# Note that the first set of 2 operations and the second set of a single operation
# means the same.
#
#   r1 = Rangeary(nil..Float::INFINITY).conjunction( RangeExtd::NONE )
#     # => Rangeary(RangeExtd::NONE)
#   r2 = r1.negation
#     # => Rangeary(nil..Float::INFINITY)
#
#   ~(Rangeary(nil..Float::INFINITY) * RangeExtd::NONE)
#     # => Rangeary(nil..Float::INFINITY)
#
# In the second example below, a negative infinity of "+d+" is explicitly specified
# for a Range of single alphabet String.
#
#   Rangeary("f".."k", negative: "d").negation
#     # => Rangeary("d"..."f", "k"<..nil)
#
# where +"k"<..nil+ means a begin-exclude Range or +RangeExtd("k"..nil, true)+.
#
#
# === Algorithm of determining default infinities
#
# Callers can supply user-defined infinity objects for both or either
# positive and negative infinity and in that case they are accepted
# as the infinities with the highest priority, though ArgumentError might be
# issued if they contradict the elements; for example, if a {Rangeary}
# instance consists of an array of Integer Ranges (RangeExtd) like +(3..8)+,
# and yet if String "abc" is specified as an infinity, it *contradicts*
# the elements in the sense they are not comparable.
#
# Internally, the {Rangeary} instance has a Hash extended with {Rangeary::Util::HashInf},
# which can be obtained with {Rangeary#infinities}.
# It has only 2 keys of +:negative+ and +:positive+, the values of which
# are the current best-guessed or definite infinities.  The Hash also
# holds status information for each polarity with 3 levels of
#
# 1. <tt>false</tt>
# 2. <tt>:guessed</tt>
# 3. <tt>:definite</tt>
#
# It is +false+ only when the Rangeary is absolutely void with no
# information about the contents: +Rangeary(RangeExtd::NONE)+.
#
# If the user explicitly specifies a boundary in the optional arguments in
# initialization of {Rangeary}, it is accepted in principle with an associated status of <tt>:definite</tt>.
#
# If the user-specified main arguments in initialization contain
# a (potentially multiple) {Rangeary}, their defined infinities are
# inherited with their associated statuses.
#
# Also, user-supplied Range-s or RangeExtd-s to the arguments in
# initialization of {Rangeary} always have, except for
# +RangeExtd::NONE+, concrete boundary values, which can be +nil+.
#
# If one of the boundaries of a Range (n.b., it is *not* Rangeary) contains either +nil+ or one of infinite values
# (which is checked with +RangeExtd::Infinity.infinite?+, where in practice
# a duck-typing check is performed, using the method +infinite?+), then
# it is accepted as an infinite value with an associated status of <tt>:definite</tt>.
#
# Otherwise,
#
# 1. if a boundary value is a (real-type) Numeric, +Float::INFINITY+ (or
#    its negative,
# 2. or otherwise, +nil+
#
# is set as an infinity of the boundary with an associated status of +:guessed+.
#
# Note that the priority used to be different up to Rangeary Ver.1; +nil+ was not used and
# instead the +RangeExtd::Infinity+ objects were used. It was because
# the beginless Range (and endless Range before Ruby-2.6) has not been defined before
# Ruby 2.7.  Now they are defined, it is only natural to use +nil+ as the
# default infinities in both ends, hence the change in specification in
# {Rangeary} Ver.2.
#
# Usually the arguments given in initialization of a {Rangeary} contain
# more than one set of infinities candidate, unless only a single
# argument of either Range (or its subclass instance) with no optional
# arguments is given.  The priority is judged in the following order:
#
# 1. the optional arguments
# 2. an associated status of <tt>:definite</tt>, <tt>:guessed</tt>, and
#    <tt>false</tt> in this order
#
# If the associated statuses are equal for two or more inputs, the most
# extreme one among them for each polarity is chosen.  For example, suppose
# two instances of {Rangeary} are given in initialization of another
# {Rangeary} and their negative infinities are "+b+" and "+c+".  Then,
# because of
#
#   "b" < "c"
#
# the former ("+b+") is adopted as the new negative infinity.  Note that
# the parameters given in the optional arguments have always higher
# priority regardless.
#
#
# The following examples demonstrate the specification.
#
#   Rangeary(7..).negation
#     # => Rangeary(-Float::INFINITY...7)
#   Rangeary(7..).negation.negation
#     # => Rangeary(7..)
#
# Remember the default infinity for Float is +Float::INFINITY+. In this
# case, however, the positive infinity was in practice specified by the
# user to be +nil+ in the form of argument of +(7..)+ If you want to
# specify the negative infinity instead, you must do it explicitly:
#
#   Rangeary(7.., negative: nil).negation
#     # => Rangeary(...7)
#
# Alternatively, you can always use cojunction like (the following two mean the same):
#
#   Rangeary(..nil).conjunction(Rangeary(7..)).negation
#     # => Rangeary(...7)
#   ~(Rangeary(..nil) * Rangeary(7..))
#     # => Rangeary(...7)
#
# The registered infinities for each instance is obtained (Hash extended with
# HashInf), which has
# two keys of +:positive+ and +negative+, with the method {#infinities};
# for example,
#
#   ran.infinities
#     # => <Hash(Inf): {:negative=>"a", :positive=>nil},
#     #     status: {:negative=>:definite, :positive=>:guessed}>
#
# Note that the values of the returned Hash (+HashInf) may be +false+;
# if it is not convenient, call it as +#instances(convert: true)+
# with which +false+ in the returned value, if there is any, is converted
# to +nil+ and the standard Hash as opposed to
# Hash extended with {Rangeary::Util::HashInf} is returned:
#
#   ran.infinities(convert: true)  # => { :negative => "a"
#                                  #      :positive => nil, }
#
class Rangeary < Array
  undef_method :*, :+, :length, :reverse

  # Use Module both for class and instance methods
  extend  Rangeary::Util
  include Rangeary::Util

  # # Hash with the keys of :negative and :positive  => becomes a method in Ver.1
  # attr_reader :infinities

  # Constructor
  #
  # Arbitrary (positive) number of arguments of {Rangeary} or Range or its subclasses can be given.
  # +(r1, [r2, ...])+
  #
  # === Note for Developers about the infinities
  #
  # Instance variable +@infinities+ (Hash extended with {HashInf}) is defined.
  # Method {#infinities} returns it.
  #
  # Basically it has the positive and negative infinities AND their confidence levels.
  # For example, a Numeric Range can have three or more types of infinities of
  # +nil+, +Float::INFINITY+, and +RangeExtd::Infinity+ or something a user defines.
  # Since it is not trivial to determine the infinities, they are usually implicitly
  # guessed from Range/RangeExtd and in such cases the confidence level is not high.
  # See {HashInf} for detail.
  #
  # In initialization, you can give a Hash+{Rangeary::Util::HashInf} infinities object directly
  # at the end of the main arguments instead of the two keyword arguments
  # (the latter of which have a higher priority if both are specified, but you should
  # avoid such uses).  However, in that case, no other information
  # about the main argument will be considered and so you should use it
  # with extreme caution, or don't use it unless you know exactly what you are doing.
  #
  # @example Detailed example with explicit infinities, negation(~), disjunction(+), conjunction(*), exclusive disjunction(^)
  #
  #    ran2 = ("f".."k")
  #    rae1 = RangeExtd("k"..nil, true)
  #    r3 = ~(Rangeary(ran2, negative: "d"))
  #      # => Rangeary("d"..."f", rae1) == Rangeary("d"..."f", "k"<..nil)
  #    r3.infinities[:negative]  # => "d"
  #    r3*(..nil)  # => r3,
  #
  #    r4 =   Rangeary(r3, negative: "a")
  #    Rangeary(r4, negative: "b").infinities[:negative]  # => "b"
  #
  #    r5 =  ~Rangeary(r4)
  #      # => Rangeary("a"..."d", "f".."k")
  #
  #    r6 = r3 + Rangeary("c".."d", negative: "a")  # disjunction
  #      # => Rangeary("c"..."f", "k"<..nil)
  #
  #    r7 = r3 * Rangeary("c".."d", negative: "a")  # conjunction
  #      # => Rangeary("d".."d"), r7
  #    r7.infinities[:negative]  # => "a"
  #
  #    r8 = r3 ^ (?e..?h)                           # exclusive disjunction (XOR)
  #      # => Rangeary("d"..."e", "f".."h", "k"<..nil)
  #
  # @param inarall [Rangeary, Range, RangeExtd, Array<Rangeary, Range>] An arbitrary number of either {Rangeary} or Range (or its subclasses, notably RangeExtd).
  # @option positive [Object] :positive Object for positive infinity. In default Float::INFINITY for comparable Numeric or else nil. You may specify +RangeExtd::Infinity::POSITIVE+ or else.
  # @option negative [Object] :negative Object for negative infinity. In default -Float::INFINITY for comparable Numeric or else nil. You may specify +RangeExtd::Infinity::NEGATIVE+ or else.
  def initialize(*inarall, positive: false, negative: false)
    if inarall.last.respond_to?(:definite?) && inarall.last.respond_to?(:guessed?)
#print "DEBUG01:init: inarall[-1]=";p [inarall[-1], inarall[-1].respond_to?(:status)]
      flag_skip_adjut_infinity = true
      @infinities = inarall.pop  # inarall is destructively modified.
    else
      @infinities = HashInf.construct()
    end
    @infinities.merge_hashinf!({ negative: negative, positive: positive }, force: true)
    #@infinities =  { :negative => nil, :positive => nil }
    #opts = {}.merge @infinities  ########## redundant!
#print "DEBUG02:init: @infinities @inarall(true)=";p [@infinities, @infinities.respond_to?(:status)]

    if inarall.size < 1
      super [RangeExtd::NONE]  # Since Ver.1
      return
      # raise ArgumentError, "wrong number of arguments (#{inarall.size} for 1 or more)."
    end

    # This uses information of {Rangeary#infinities} in the arguments if there ia any.
    @infinities = _build_infinities(@infinities, self.class.flatten_no_rangeary(*inarall)) if !flag_skip_adjut_infinity # set @infinities (wherever possible)

#print "DEBUG03:init: @infinities @inarall(true)=";p [@infinities, @infinities.respond_to?(:status)]
#print "DEBUG04:init: inarall=";p inarall
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
#print "DEBUG05:init: in_ranges=";p in_ranges
#print "DEBUG06:init: in_ranges[1].is_none?=";p in_ranges[1].is_none?

    ## NOTE: Simple map(&:infinities) is not ideal here, because map(&:infinities) tries to
    ##   supplement when the value is nil.  However, in self.conjunction(), it uses map(&:infinities),
    ##   which is not ideal.
    #inherited_infs = inarall.find_all{|i| self.class.send(:is_rangeary_type?, i)}.map{ |ec|
    #  ec.instance_variable_get(:@infinities)
    #}

    #@infinities = _build_infinities(in_ranges, inherited_infs: inherited_infs, guess_strict: false, **opts) # set @infinities (wherever possible)

    # Call _merge_overlaps
    begin
#print "DEBUG07:init: afcon=";p [convert2range(in_ranges), convert2range(in_ranges)[-1].is_none?]
      arRange = _merge_overlaps( convert2range(in_ranges) )
    rescue => err
      # Trap just to change the type of the exception.
      raise ArgumentError, err.message, err.backtrace
    end
#print "DEBUG09:init: arRange=";p arRange

    ### I don't know why this was set...  Empty Rangeary should be allowed, like an empty Array.
    #if arRange.empty?
    #  raise ArgumentError, 'no significant argument given for Rangeary.'
    #end

    ## Setting @infinities[:negative, :positive]
    # get_infinities_legacy(arRange, hsInheritedObj, hsInheritedAry, hsInheritedClass, **opts)

    super(arRange)
    self.freeze
    _validate_infinities
#print "DEBUG99:init: @infinities @inarall(true)=";p [@infinities, @infinities.respond_to?(:status)]
  end	# def initialize(*inarall, positive: false, negative: false)


  # Validates the given infinities
  #
  # Negative should be smaller than any and positive be larger.
  #
  # If a new infinity is an infinity (respond_to? infinite?) and has
  # the right polarity, or if it is nil, it is automatically accepted.
  # For example, the positive Infinity can be defined as +RangeExtd::Infinity::POSITIVE+
  # even when the existing +Rangeary#begin+ is +Float::INFINITY+
  #
  # Theoretically, the following policy would be possible, but it is not adopted:
  #
  # # Allowing to specify Rangeary(..?d, negative: ?a, positive: ?z),
  # # because nil is basically "undefined" rather than infinite.
  #
  # @return [void]
  # @raise [ArgumentError] if they are contradictory.
  def _validate_infinities
    neg, pos = @infinities[:negative], @infinities[:positive]
    #return if [neg, pos].include?(nil) || [neg, pos].include?(false)

    msg = "specified/inherited negative and positive infinities are contradictory (such as reversed polarities or a combination of Float::INFINITY and RangeExtd::Infinity): [n,p]=#{[neg, pos].inspect}"
    begin
      if ![neg, pos].include?(nil) && ![neg, pos].include?(false) && neg > pos
        raise ArgumentError, msg
      end
    rescue ArgumentError
      # Likely, Float::INFINITY and RangeExtd::Infinity with the opposite polarities
      # are specified, or the former is guessed from a Range.
      raise ArgumentError, msg
    end
    return if empty_element?

    fmt = "specified/inherited %s infinity (%s) is not %s enough or inconsistent: (<=> %s)"
    hsp = {
      negative: {
        sm_lg: "small",
        metho: :begin,
        oper:  :>   # OK if self.begin > new-infinity(:negative)
      },
      positive: {
        sm_lg: "large",
        metho: :end,
        oper:  :<   # OK if self.end < new-infinity(:positive)
      },
    }

    # Gets a non-nil non-infinity boundary value if there is any, else +false+.
    # NOTE self.begin|end is never false, because RangeExtd would not accept it.
    valcmp =
      if (self.begin.nil? || RangeExtd::Infinity.infinite?(self.begin))
        if (self.end.nil? || RangeExtd::Infinity.infinite?(self.end))
          false
        else
          self.end
        end
      else
        self.begin
      end

    hsp.each_pair do |posneg, hsval|
      ea_inf = @infinities[posneg]  # Infinity value
      ea_val = self.send(hsval[:metho])  # Current Range boundary value to compare with
      msg = sprintf fmt, posneg.to_s, ea_inf.inspect, hsval[:sm_lg], ea_val.inspect
      is_pol = (posneg.to_s+"?").to_sym  # either :positive? or :negative?
      next if false == ea_inf  # infinity undefined (uninitialized).
      next if ea_inf.nil?
      next if ea_val == ea_inf
      begin
        if RangeExtd::Infinity.infinite?(ea_inf) && ea_inf.respond_to?(is_pol) && ea_inf.send(is_pol)
          # To swap RangeExtd::Infinity <=> Float::INFINITY is allowed as long as
          # the element of the range does not contradict it, e.g.,
          # setting Float::INFINITY as an infinity is not allowed for a String Range.
          next if false == valcmp
          next if (ea_inf <=> valcmp)  # namely if it is non-nil (0, 1, or 2)
          raise ArgumentError, posneg.to_s.capitalize+" infinity not comparable with (#{valcmp})."
        end
        next if !ea_val.nil? && ea_val.send(hsval[:oper], ea_inf)
      rescue ArgumentError
        # raises an exception as follows
      end
      raise ArgumentError, msg
    end

    #msg = sprintf fmt, "negative", neg.inspect, "small", self.begin.inspect
    #begin
    #  if neg.nil? ||
    #     false == neg ||
    #     self.begin.nil? ||   # Allowing to specify Rangeary(..?d, negative: ?a, positive: ?z), because nil is basically "undefined" rather than infinite.
    #     self.begin == neg ||
    #     self.begin >  neg
    #     #(RangeExtd::Infinity.infinite?(neg) && neg.negative?) ||
    #    # OK
    #  else
    #    raise ArgumentError, msg
    #  end
    #rescue ArgumentError
    #    raise ArgumentError, msg
    #end

    #msg = sprintf fmt, "positive", pos.inspect, "large", self.end.inspect
    #begin
    #  if pos.nil? ||
    #     false == pos ||
    #     self.end.nil? ||   # Allowing to specify Rangeary(..?d, negative: ?a, positive: ?z), because nil is basically "undefined" rather than infinite.
    #     self.end == pos ||
    #     self.end <  pos
    #     #(RangeExtd::Infinity.infinite?(neg) && neg.negative?) ||
    #    # OK
    #  else
    #    raise ArgumentError, msg
    #  end
    #rescue ArgumentError
    #    raise ArgumentError, msg
    #end
  end
  private :_validate_infinities


  # Returns Hash (+{HashInf}) of the currently defined infinities of self
  #
  # The returned hash has two keys of +:positive+ and +:negative+ with infinity values.
  #
  # Internally, the infinities (Hash, extended with {HashInf}) are usually "guessed"
  # from the provided Range etc and inherited. In the extreme case of
  #
  #    Rangeary.new(RangeExtd::NONE)
  #
  # they remain undefined (with the values of the Hash being +false+orown) because there is no way to
  # guess what its infinities are, in which case they are open to change,
  # depending on following operations.
  #
  # If the optional argument of (+convert: true+) is given,
  # any +false+ values are converted to something
  # appropriate for the situation (that is, +nil+) before the value is returned.
  # Also, the returned value is a standard Hash as opposed to Hash+HashInf.
  #
  # @param convert [Boolean] if true (Def: false), return never false, else +@infinities+
  # @return [Hash] keys of :positive and :negative.  The values are never false.
  def infinities(convert: false)
    convert ? _get_significant_infinities(@infinities) : @infinities
  end

  # Returns an infinities Hash with both ends being signifincant (never false, but maybe nil)
  #
  # Note this returns a standard Hash as opposed a Hash extended with {Util::HashInf}
  # and the Hash contents of the returned value is modified only when
  #   @infinities.status_is_nil?(posneg)
  # is true for +posneg+ of +:negative+ and +:positive+.
  #
  # @param ininf [Hash] keys of :positive and :negative.
  # @return [Hash] keys of :positive and :negative.  The values are never false.
  def _get_significant_infinities(ininf)
    if    false == ininf[:positive]
      case ininf[:negative]
      when false
        return( {negative: nil, positive: nil} )
      when -Float::INFINITY
        return ininf.merge({positive: Float::INFINITY})
      when RangeExtd::Infinity::NEGATIVE
        return ininf.merge({positive: RangeExtd::Infinity::POSITIVE})
      else  # including nil
        return ininf.merge({positive: nil})  # Default: nil
      end
    elsif false == ininf[:negative]
      # positive is never false.  See above
      case ininf[:positive]
      when Float::INFINITY
        return ininf.merge({negative: -Float::INFINITY})
      when RangeExtd::Infinity::POSITIVE
        return ininf.merge({negative: RangeExtd::Infinity::NEGATIVE})
      else  # including nil
        return ininf.merge({negative: nil})  # Default: nil
      end
    else
      # Both ends are defined.
      {}.merge ininf
    end
  end
  private :_get_significant_infinities


  # If self covers the entire range?
  #
  # @note I realise this contradicts +Enumerable#all?+, which Array includes.
  #    This method may be removed in the future release...
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

  # Origiinal +Array#===+
  alias_method :triple_equals_orig, :=== if ! self.method_defined?(:triple_equals_orig)

  # True if the inObj is in the range.
  #
  # This method works on the basis of each element, that is,
  # if for any of the +RangeExtd+ in the +Rangeary+, +RangeExtd#===+
  # returns true, this will return true.  That means, if the argument is
  # a +Rangeary+ (or +Range+) object, this always returns false.
  # Note +#include?+ and +#member?+ work the same as in the standard Array, 
  # whereas {#include_element?} and {#member_element?} are the alias of 
  # this method.
  #
  # See {#cover?}.  The difference between this method and {#cover?} is
  # the same as that in Range.
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


  # @return [Object]  The +RangeExtd#begin+ of the first +RangeExtd+.
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
  # the same as that in Range.
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


  # If the range defined in this object is empty (as in +Range#empty?+), returns true.
  # 
  def empty_element?
    each do |er|
      if ! er.empty?
        return false
      end
    end
    return true
  end


  # @return [Object]  The +RangeExtd#end+ of the last +RangeExtd+.
  def end()
    if to_a.size > 0
      to_a[-1].end
    else
      nil
    end
  end
  alias_method :end_element, :end


  # +Range#equiv?+ method, defined in range_extd library, extended to this {Rangeary}.
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


  # Returns the first n elements of the entire range, the same as +Range#first+.
  # 
  # If the argument is not given, this simply calls +Range#begin+ for the first
  # +RangeExtd+.
  # 
  # If not, and if the elements in the ranges are not discrete, like Float,
  # an exception is raised (see +Range#first+).
  # Note this works on the element basis, being different from {Rangeary#first},
  # which works on the array basis.
  # 
  # @param n [Integer] (Optional) positive.
  # @return [Object] equivalent to {#begin} if no argument is given.
  # @return [Array] Array of the first n elements in the range.
  # @raise [TypeError] if the ranges has no iteration defined, such as, starting from Float.
  def first_element(n=nil)
    if n.nil?
      self.first.first
      #self.begin()
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


  # Returns the last n elements of the entire range, the same as +Range#last+.
  # 
  # If the argument is not given, this simply calls +Range#end+ for the last
  # +RangeExtd+.
  # 
  # If not, and if the elements in the ranges are not discrete, like Float,
  # an exception is raised (see +Range#last+).
  # Note this works on the element basis, being different from +Rangeary#last+,
  # which works on the array basis.
  # 
  # @param n [Integer] (Optional) positive.
  # @return [Object] equivalent to {#end} if no argument is given.
  # @return [Array] Array of the last n elements in the range.
  # @raise [TypeError] if any of the ranges has no iteration defined, such as, starting from Float.
  def last_element(n=nil)
    if n.nil?
      self.last.last  # self.end() would behave differently when the last Range is like (2..)
    elsif n < 0
      raise ArgumentError, "the argument #{n} has to be positive."
    else
      arRet = []
      m = n
      to_a.reverse.each do |eachr|
        ar = eachr.last(m)

        ## Dealing with a bug of Range#last(n):
        #    https://bugs.ruby-lang.org/issues/18994
        #  The bug appears in Ruby 2.7.0 onwards (at least up to Ruby-3.1.2)
        if m > 0 && ar.count < [m, eachr.count].min
          ar = eachr.to_a
        end

        arRet.unshift(*ar)  # arRet = ar + arRet
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


  # Return the sum of +RangeExtd#size+ of all the ranges in the object.
  #
  # @return [Integer]  0 if +RangeExtd::NONE+
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
  # == Deverloper's note
  #
  # Handling of infinities is a little tricky.
  #
  # If nothing was considered,
  #   Rangeary(6..nil) - (6...8)
  # would return 
  #   Rangeary([8..Float::INFINITY])
  # because the first step of operation, negation of +(6...8)+, would
  # contain +Float::INFINITY+ and then the standard conjunction would regard it
  # as the *definite* sign of +Float::INFINITY+ being the correct infinity,
  # as opposed to +nil+.
  #
  # In the case, only the infinity that appears in the user-supplied formula is +nil+.
  # Therefore, at least the positive infinity should be recognized as +nil+,
  # whereas the negative infinity remains uncertain.
  #
  # @param r [Rangeary, RangeExtd, Range]
  # @return [Rangeary] 
  def subtraction(r)
    infs = _get_infinities_from_multiple(self, r)
    conjunction_core(self, Rangeary.new(r, **infs).negation, infs: infs)
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

  ## Internal version of Conjunction, where +infinities+ can be given.
  ##
  ## @param r [Rangeary, RangeExtd, Range]
  ## @param infs [Hash] ({HashInf}) if supplied, +infinities+ are NOT guessed from the main arguments. 
  ## @return [Rangeary] 
  #def conjunction_core(r, infs: nil)
  #  self.class.conjunction_core(self, r, infs: infs)
  #end
  #private :conjunction_core


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

      # ALL -> NONE (specifying infinity parameters (positive and negative) is important.)
      return Rangeary.new(RangeExtd::NONE, @infinities) if RangeExtd::ALL == eachr
      #return Rangeary.new(RangeExtd::NONE, :positive => @infinities[:positive], :negative => @infinities[:negative]) if RangeExtd::ALL == eachr

      # null(NONE) -> ALL
      #
      # Note there should be no more than 1 null Range/RangeExtd in Rangeary.
      if eachr.null? 
#print "DEBUG:242:neg: eachr=";p eachr
        begin
          _ = 1.0 * eachr.begin
#print "DEBUG:243:neg: @infinities=";p @infinities
          return Rangeary.new(-Float::INFINITY..Float::INFINITY, @infinities)
        rescue TypeError	# XXXX can't be coerced into Float
          return Rangeary.new(infinities(convert: true)[:negative]..infinities(convert: true)[:positive], @infinities)
          #return Rangeary.new(infinities[:negative]..infinities[:positive], positive: @infinities[:positive], negative: @infinities[:negative])
        end
      end

      # ea_b = eachr.begin  ###
      eachm = _comparable_beginend(eachr)  # For Ruby-2.7 Beginless Range
      ea_b = eachm.begin
#print "DEBUG:245:neg: bef:ea_b=";p ea_b
      #if (RangeExtd::Infinity::NEGATIVE == ea_b)	# Including -Float::INFINITY and other general negative infinities (nb., [#==] is not commutative!).
      if (RangeExtd::Infinity.infinite?(ea_b) && ea_b.respond_to?(:negative?) && ea_b.negative?) || infinities[:negative] == ea_b
        # The existing first range starts from the negative infinity, so skip this one.
#print "DEBUG:246:neg: 1st:ea_b=";p ea_b
      else
        if prevend.nil?
          # The returned first range starts from the negative infinity.
          ran_tmp = normalized_range_for_negation(infinities(convert: true)[:negative], eachr.begin)
#print "DEBUG:247:neg: 2nd:ran_tmp=";p ran_tmp
          if RangeExtd::NONE != ran_tmp
            # Avoid (inf..inf) type, which would cause RangeError (in Ruby 2.6) anyway.
            arran.push( RangeExtd(ran_tmp, :exclude_end => (! eachr.exclude_begin?)) )
          end
        else
          arran.push( RangeExtd(prevend_orig, eachr.begin, :exclude_begin => (! prevst), :exclude_end => (! eachr.exclude_begin?)) )
#print "DEBUG:248:neg: 2nd:arran=";p arran
          # arran.push( RangeExtd(prevend, eachr.begin, :exclude_begin => (! prevst), :exclude_end => (! eachr.exclude_begin?)) )
        end
      end	# if (eachr.begin == -Float::INFINITY) || (RangeExtd::NONE == eachr.begin)
      prevend_orig = eachr.end
      prevend = _comparable_end(eachr)  # For Ruby-2.6 Endless Range
      prevst  = eachr.exclude_end?
    end		# to_a.each do |eachr|
#print "DEBUG:249:neg: 1end:n=";p [prevend_orig,prevend,prevst]

    #if (RangeExtd::Infinity::POSITIVE == prevend)	# Including Float::INFINITY and other general positive infinities (nb., [#==] is not commutative!).
    if (RangeExtd::Infinity.infinite?(prevend) && prevend.respond_to?(:positive?) && prevend.positive?) || infinities(convert: true)[:positive] == prevend
      ## Do nothing
    else
      rbeg = (prevend_orig || prevend)
      ran_tmp = normalized_range_for_negation(rbeg, infinities(convert: true)[:positive])

#print "DEBUG:260:neg: 2beg:";p [rbeg, ran_tmp]
      if RangeExtd::NONE != ran_tmp
        # Avoid (inf..inf) type, which would cause RangeError (in Ruby 2.6) anyway.
        arran.push( RangeExtd.new(ran_tmp, :exclude_begin => (! prevst)) )
      end
    end
#print "DEBUG:262:neg: arran:";p arran

    #Rangeary.new(arran, :positive => infinities[:positive], :negative => infinities[:negative])
    #Rangeary.new(arran)  ################ infinities
    #Rangeary.new(arran, positive: @infinities[:positive], negative: @infinities[:negative])
    Rangeary.new(arran, @infinities)  # @infinities is inherited as it is.
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
    conjunction_core(r1, r2)
  end


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
  # @note This method becomes a subset of {Rangeary.comparable_beginend} in
  #    Rangeary Ver.2, supporting Ruby-2.7 Beginless Ranges.
  #
  # @param ran [Range, RangeExtd]
  # @param infinities [Hash<Boolean>] two-elements of :positive and :negative
  # @return [Object]
  # @raise [RangeError] if ran is not Range#valid?
  def self.comparable_end(ran, infinities: nil)
    comparable_beginend(ran, infinities: infinities).end

    #if ran.class.method_defined?(:is_none?) && ran.is_none?
    #  nil
    #elsif ran.end.nil? && ran.begin.nil?
    #  nil
    #elsif ! ran.end.nil?
    #  ran.end  # Before Ruby 2.6, this is always the case.
    #elsif ran.begin.class.method_defined? :to_int
    #  Float::INFINITY
    #else
    #  RangeExtd::Infinity::POSITIVE
    #end
  end

  # Returns a two-element Array with comparable elements
  #
  # The Endless and Beginless Ranges were introduced in Ruby-2.6
  # and Ruby-2.7, respectively.
  # In Ruby's definition, the Endless/Beginless Ranges take a form of
  # +(5..nil)+ and +(nil..5)+ (they can be expressed without "nil")
  # and accordingly its "end" (n.b., +last+ raises +RangeError+) and
  # "begin" are nil:
  #
  #   (5..nil).end    # => nil
  #   (nil..5).begin  # => nil
  #
  # Then, when you compare the "ends" of two Ranges with the method +<=>+,
  # the result is true (it may use to be nil??).
  #
  # In fact, in Ruby, two Range objects are not comparable with eath other,
  # though the comparator operator (or method) +<=>+ is defined for Range,
  # presumably inherited from Object:
  #
  #   (3..6) <=> (8..9)  # => nil
  #   (3..6) <=> (3..6)  # => 0
  #
  # (Note this default behaviour is not unreasonable given what one compares
  # is equivocal for Ranges, for example, the size, start or end?
  # Indeed Range does not have a method +Range<=+ and hence +<=>+ is meaningless.
  # except for the equality, where 0 is returned.)
  #
  # Then, when Range#end returns just nil, which should be interpreted
  # as *Endless* in Range's context, it does not matter for Range.
  # However, it is inconvenient for this class, {Rangeary}!
  # The heart of comparison in {Rangeary} is +<=>+ for the begin first,
  # and then the end if the first comparison results in equal.
  #
  # This method returns a +RangeExtd+, in which +Range#begin+ and +Range#end+
  # are converted into something comparable when possible.
  # More specifically, unless the given Range is +RangeExtd::NONE+,
  # if the "begin" and/or "end" of the given Range is nil,
  # the value is converted into
  # Infinity (one of +Float::INFINITY+ or +RangeExtd::Infinity::POSITIVE+
  # or their negatives, or user-specified infinities, where +nil+ is ignored)
  # for the respective value. Else, simply
  # +RangeExtd(Range#begin, Range#end)+ is returned.
  #
  # Note that because the returned value is RangeExtd, which does not
  # accept any Ranges that are not Range#valid, if such an invalid Range
  # is given RangeError is raised.  This includes Ranges that contains
  # +RangeExtd::Nowhere::NOWHERE+ (except for +RangeExtd::NONE+).
  #
  # The optional argument of Hash +infinities+ having boolean values for
  # two keys of :positive and :negative can be given, in order to
  # explicitly specify your infinities.
  #
  # @overload self.comparable_beginend(ran, infinities: nil)
  #   Range or RangeExtd
  #   @param ran [Range, RangeExtd]
  #   @param infinities [Hash<Boolean>] two-elements of :positive and :negative
  # @overload self.comparable_beginend(ran, vend, infinities: nil)
  #   Begin and End values for Range
  #   @param ran [Object]
  #   @param vend [Object]
  #   @param infinities [Hash<Boolean>] two-elements of :positive and :negative
  #
  # @return [RangeExtd] exclude statuses for begin and end follow the input.
  # @raise [RangeError] if ran is not Range#valid?
  def self.comparable_beginend(ran, vend=Object, infinities: nil)
    comparable_beginend_core(ran, vend, infinities: infinities)  # In Rangeary::Util
  end

  # Similar to {Rangeary.comparable_end} but for +begin+.
  #
  # Introduced for Ruby-2.7 beginless Range.
  #
  # @param (see Rangeary.comparable_beginend)
  # @return [Object]
  # @raise [RangeError] if ran is not Range#valid?
  def self.comparable_begin(ran, infinities: nil)
    comparable_beginend(ran, infinities: infinities).begin
  end

  # Returns the array sorted, based on (1) the begin objects and their boundary state,
  # (2) then the end objects and their boundary state.
  # +RangeExtd::NONE+ comes first, if included in the input array.
  #
  # @param ar [<Range, RangeExtd>] Arbitrary number.
  # @return [Array<Range, RangeExtd>]
  def self.sort_ranges(*ar)
    sort_ranges_core(*ar)  # In Rangery::Util
  end

  # Return a flattened Array where {Rangeary} is not flattened.
  #
  # @param arin [Array<Range, RangeExtd, Rangeary, Array>]
  # @return [Array<Range, RangeExtd, Rangeary>]
  def self.flatten_no_rangeary(*arin)
    flatten_no_rangeary_core(*arin)
  end

  ####################
  private
  ####################

  # Called from {Rangeary.initialize}
  def convert2range(inarall)
#print "DEBUG101:conv: inarall=";p [inarall,inarall[-1].is_none?]
    inarall.flatten.map{|i|
      if defined? i.first_element
        i.to_a
      else
        i
      end
    }.flatten.map{|j|
#print "DEBUG108:conv: j=";p j
      if (defined? j.exclude_begin?)
        j
      else
#print "DEBUG108:conv: rj=";p RangeExtd(j)
        RangeExtd(j)
      end
    }
  end

  # Instance method version
  #
  # where @infinities are taken into account.
  #
  # @param ran [Range, RangeExtd]
  # @return [Object]
  def _comparable_end(ran)
    _comparable_beginend(ran).end

    #if ran.class.method_defined?(:is_none?) && ran.is_none?
    #  nil
    #elsif ran.end.nil? && ran.begin.nil?
    #  nil
    #elsif ! ran.end.nil?
    #  ran.end  # Before Ruby 2.6, this is always the case.
    #else
    #  infinities[:positive] || RangeExtd::Infinity::POSITIVE
    #end
  end
  private :_comparable_end

  # Same as {#_comparable_end} but for begin
  #
  # Required for Ruby-2.7 and beyond.
  #
  # This was introduced for Ruby-2.6 even though this is
  # beyond the scope of Ruby-2.6, because it is
  # needed for {#negation}.
  #
  # @param rbeg [Object]
  # @param rend [Object]
  # @return [Object]
  def _comparable_begin(rbeg, rend)
    _comparable_beginend(rbeg, rend).begin

    #if rend.nil? && rbeg.nil?
    #  nil
    #elsif ! rbeg.nil?
    #  rbeg
    #elsif rend.class.method_defined? :to_int
    #  -Float::INFINITY
    #else
    #  RangeExtd::Infinity::NEGATIVE
    #end
  end
  private :_comparable_begin


  # Instance method version
  #
  # where @infinities are taken into account.
  #
  # @overload _comparable_beginend(ran)
  #   Range or RangeExtd
  #   @param ran [Range, RangeExtd]
  # @overload _comparable_beginend(ran, vend)
  #   Begin and End values for Range
  #   @param ran [Object]
  #   @param vend [Object] 
  #  
  # @return [RangeExtd] exclude statuses for begin and end follow the input.
  # @raise [RangeError] if ran is not Range#valid?
  def _comparable_beginend(ran, vend=Object)
    self.class.comparable_beginend(ran, vend=vend, infinities: infinities)   ###################
  end
  private :_comparable_beginend

  # Build the instance variable (Hash) @infinities
  #
  # @param opts [Hash] :positive and :negative Object of infinities.
  # @param arin [Array<Range, RangeExtd, Rangeary>]
  # @param guess_strict [Boolean] if true, make only strict guess for infinities, namely, unless the existing elements contain "infinity"-type objects, leave it false (currently unsupported) (it is passed to {#_best_guessed_infinities}(strict: false) --- currently not used)
  # @return [Array] guessed @infinities
  def _build_infinities(opts, arin, guess_strict: false)
  # @param inherited_infs [Array<Hash<Infinity, nil>>] Inherited infinities from the input Rangeary-s
  #def _build_infinities(arin, inherited_infs: [], guess_strict: false, **opts)

    reths = _get_infinities_from_multiple(arin)

#    reths = HashInf.construct
#    #hsref = {}.merge reths  # Standard Hash (not extended with HashInf)
#
#    arin.each do |rang|  # Range, RangeExtd, Rangeary
##print "DEBUG61:get: rang=";p rang
#      _validate_opts_infinities([rang], infs: reths)
#      reths.merge_hashinf!(_get_infinities_from_obj(rang))
#    end
    #  reths, hs = _merged_infinities(reths, rang)
    #  hsref, _  = _merged_infinities(hsref, hs) unless hs.empty?  # Gussed one from Range
#print "DEBUG61:get: [reths,hs,hsref]=";p [reths,hs,hsref]
    #end
#print "DEBUG62:get: [reths,opts]=";p [reths,opts]
    #reths, _ =    _merged_infinities(hsref, reths)  # Those taken from argument Rangeary has a higher priority (except for the value of FALSE)


    reths.merge_hashinf!(opts, force: true)
#print "DEBUG63:get: [reths]=";p [reths]

    ## Read @infinities from Rangeary-s in arin, if exits
    ##reths = self.class.send(:_validate_select_infinities, inherited_infs, reths)  ###############
    ##_ = self.class.send(:_validate_select_infinities, [reths], @infinities) # Just to check
    #_ = _validate_select_infinities([reths], @infinities) # Just to check

    reths

    ## Read @infinities from Rangeary-s in arin, if exits
    #return _merged_infinities(reths, opts).first  # Optional argument in new() has the highest priority


#    reths = _build_infinities_from_opts(opts)  # reths.nil? if no options are specified.
##print "DEBUG62:get: reths=";p reths
#    if reths
#      _validate_opts_infinities(arin, infs: reths)
#      # return reths if (reths.all?{ |i| i[1] })
#    end
#    leave_existing = !!reths
#
#    hs = {}.merge(@infinities)
#    hs.each_key {|k| hs[k] = nil }  # :positive, :negative => nil
#    reths ||= hs
#
#    # Read @infinities from Rangeary-s in arin, if exits
#    reths = self.class.send(:_validate_select_infinities, inherited_infs, reths)  ###############
#
#    return reths if (reths.all?{ |i| i[1] })
#    leave_existing ||= reths.any?{ |i| i[1] }
#
##print "DEBUG169:get: arin.n=";p arin[-1].is_none?
#    _best_guessed_infinities(reths, arin, strict: guess_strict, leave_existing: leave_existing)
  end
  private :_build_infinities



  # Returns @infinities from the options.
  #
  # If not specified actually, returns nil.
  #
  # @param opts [Hash] must include the sole kyes :positive and :negative
  # @return [Hash, nil]
  def _get_infinities_from_opts(opts)
    return nil            if false == opts[:positive] && false == opts[:negative]
    return {}.merge(opts) if false != opts[:positive] && false != opts[:negative]

    if false == opts[:positive]
      posi =
        if is_num_type? opts[:negative]
          Float::INFINITY
        elsif RangeExtd::Infinity.infinity? opts[:negative]
          RangeExtd::Infinity::POSITIVE
        else
          nil
        end
      return opts.merge({positive: posi})
    end

    nega =
        if is_num_type? opts[:positive]
          -Float::INFINITY
        elsif RangeExtd::Infinity.infinity? opts[:positive]
          RangeExtd::Infinity::NEGATIVE
        else
          nil
        end
    opts.merge({negative: nega})
  end
  private :_get_infinities_from_opts

  # Returns the best guess Infinity from a given value (like 5.2 or "a")
  #
  # @param key [Symbol] either :positive or :negative
  # @param val [Object] from which Infinity is guessed.
  # @param strict [Boolean] if strict, unless the value is a kind of Infinite, return nil
  # @return [Hash<Array>] keys (:positive and :negative) Array#size may not agree between them.
  def _guessed_infinity(key, val, strict: false)
#print "DEBUG39:guessed: key,val=";p [key, val, strict]
    return nil if !val
    return(RangeExtd::Infinity.infinite?(val) ? val : nil) if strict
    return(((key == :positive) ? 1 : -1) * Float::INFINITY) if is_num_type? val
    ((key == :positive) ? RangeExtd::Infinity::POSITIVE : RangeExtd::Infinity::NEGATIVE)
  end
  private :_guessed_infinity


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
      #ret = (_comparable_begin(rbeg, rend)..rend)  # Range
      ret = (_comparable_beginend(rbeg, rend).begin..rend)  # For Ruby 2.7+
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

  # Called from {Rangeary#initialize}.
  #
  # Process the array of RangeExtd and return the new one, in which
  # overlapped ranges are merged accordingly.
  #
  # Before returning, multiple empty Ranges are filtered out, the job
  # of which is delegated to {Rangeary#_uniq_empty_ranges}.
  # If there is no non-"empty" range, one of them will be left.
  # As a priority, an empty range with a definite class is left,
  # but if there is none, RangeExtd::NONE will be left.
  #
  # Note that (Inf..Inf) or (-Inf..-Inf) is replaced with +RangeExtd::NONE+,
  # which then will be truncated.
  #
  # @param inAr [Array<RangeExtd,Range>]
  # @return [Array<RangeExtd>]
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

#print "DEBUG67:mer: inRanges[0].is_none?=";p [inAr, inAr.map(&:is_none?)]
    inRanges = _replace_inf_inf(inAr)  # Replace meaningless inf..inf etc.
#print "DEBUG68:mer: inRanges[0].is_none?=";p [inRanges, inRanges.map(&:is_none?)]
##print "DEBUG69:mer: sorted=";p [self.class.sort_ranges(inRanges),self.class.sort_ranges(inRanges).map(&:is_none?)]
    inRanges = self.class.sort_ranges(inRanges).map{|i| (RangeExtd === i) ? i : RangeExtd(i) }	# => Rangeary.sort_ranges(ar)
#print "DEBUG70:mer: inRanges[0].is_none?=";p inRanges[0].is_none?
#print "DEBUG71:mer: inRanges=";p inRanges

    if inRanges.size < 1
      return inRanges
    end

    newRanges = [inRanges[0]]

    inRanges[1..-1].each do |eachr|
      prev = newRanges[-1]
#print "DEBUG72:mer: eachr,prev=";p [eachr,prev]

      if prev.empty? || eachr.empty?
        newRanges.push(eachr)
        next
        # All empty Ranges (potentially except for one) are removed later.
        # If all of them are empty, at least one, preferably *not* RangeExtd::NONE
        # will remain.  To maximize the possibility that one non-RangeExtd::NONE
        # empty Range remains, all of empty Ranes are pushed for now.
      end

      ## To deal with Ruby-2.6 Endless Range like (5..) (=(5..nil))
      ## *.end is guaranteed not to be false.
      #eachr_end = _comparable_end(eachr)   #############
      #prev_end  = _comparable_end(prev)   #############

      # To deal with Ruby-2.6/2.7 Endless/Beginless Ranges like (5..) (=(5..nil))
      # *.end/begin must be comparable in the following comparison.
      eachm = _comparable_beginend(eachr)  # EACH-M_odified-to-be-comparable
      prevm = _comparable_beginend(prev)

#print "DEBUG73:mer: eachm, prevm=";p [eachm, prevm]

#next if prevm.end.nil?  # the previous one is (X..nil) and this one is totally contained.

      #case eachr_end <=> prev_end   #############
      case eachm.end <=> prevm.end
      when -1	# aka, eachr_end < prev_end
        # Do nothing [Totally inclusive]
        next
#print "DEBUG74:mer: inclusive (<=>): ";p (eachm.end <=> prevm.end)
      when 0
        if (!eachr.exclude_end?) && prev.exclude_end?
          # Change the status (:exclude_end => false) for prev
          newRanges[-1] = RangeExtd.new(prev.begin, prev.end, :exclude_begin => prev.exclude_begin?, :exclude_end => false)
        else
          # Do nothing [Totally inclusive]
        end
      when 1	# aka, eachr_end > prev_end
        #case eachr.begin <=> prev_end   #############
        case eachm.begin <=> prevm.end
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
          raise RangeError "Non-comparable object in Range."  # This should not happen, unless a specific object in the Range is not comparable... like user-defined NONE??
          #newRanges.push(eachr)
        else
          raise
        end	# case eachr.begin <=> prev_end
      when nil	# aka, eachr_end > prev_end
#print "DEBUG78:mer: nil (<=>): ";p (eachm.end <=> prevm.end)
        raise RangeError "Non-comparable object in Range."  # This should not happen, unless a specific object in the Range is not comparable...
        #newRanges.push(eachr)	# must be RangeExtd::NONE (or user-defined equivalent)
      else
        raise
      end	# case eachr_end <=> prev_end 

    end		# inRanges[1..-1].each do |eachr|

#print "DEBUG79:mer: newRanges: ";p newRanges
    _uniq_empty_ranges(newRanges)
  end	# def _merge_overlaps(inAr)
  private :_merge_overlaps

  # If nil, returns +RangeExtd::Infinity+ constant
  #
  # @param val [Object]
  # @param is_positive [Boolean] true if for the positive end
  # @return [Object] non-nil object.
  def _comparable_nil(val, is_positive: true)
    return val if !val.nil?
    is_positive ? RangeExtd::Infinity::POSITIVE : RangeExtd::Infinity::NEGATIVE
  end
  private :_comparable_nil

  # Called from {Rangeary#_merge_overlaps}.
  #
  # "uniq" empty Ranges in the given Array and return the Array.
  #
  # If there is at least one non-empty range, delete all empty ranges.
  # If not, leave one of them, preferably not RangeExtd::NONE,
  # unless there is no choice.
  #
  # @param newRanges [Array<RangeExtd>] This is *destructively* modified!!
  # @return [Array<RangeExtd>]
  def _uniq_empty_ranges(newRanges)
    hsFlag = {
      :empty? => true,
      :klass  => nil,
      :found? => false,
    }

#print "DEBUG74:mer: newRanges=";p newRanges
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

#print "DEBUG75:mer: newRanges=hsFlag=";p [newRanges,hsFlag]
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
##print "DEBUG76:mer: hsFlag=";p hsFlag
    else
      # Deletes all the empty ones.
#print "DEBUG77:mer: newRanges=";p newRanges
#print "DEBUG77:mer: newRanges.none=";p newRanges[0].is_none?
      newRanges.delete_if { |er| er.empty? }
#print "DEBUG78:mer: hsFlag=";p hsFlag
    end

#print "DEBUG79:mer: newRanges=";p newRanges
    newRanges
  end   # def _uniq_empty_ranges(newRanges)
  private :_uniq_empty_ranges


  # Replaces the meaningless inf..inf Range with NONE
  #
  # @note The class information (such as Float) is discarded.
  #
  # @param arin [Array<Range, RangeExtd>]
  # @return [Array]
  def _replace_inf_inf(arin)
    arin.map{ |er|
      raise 'contact the code developer' if !defined? er.exclude_end?  # Sanity check.
      # Note: er.to_a raises RangeError for Ruby 2.6 Endless Range
      (_infinites_with_same_polarity?(er.begin, er.end) ? RangeExtd::NONE : er)

      #arran = [er.begin, er.end]
      #if (( arran.all?{ |ea| RangeExtd::Infinity.infinite?(ea) } &&
      #     (arran.all?(&:positive?) || arran.all?(&:negative?)) ))
      #    #(arran.all?(&:nil?))) # allowed in Ruby 2.7+
      #  RangeExtd::NONE
      #else
      #  er
      #end
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
  # @param ar_infs [Array] of Infinities Hash (inherited)
  # @return [Hash] each key (:(posi|nega)tive) contains a single value (potentially null) for infinity.
  def self._best_inherited_infinities(*ar_infs)
    hsar = _sort_inherited_infinities_all( _get_cand_infinities(ar_infs.flatten) )
    hsar.map{ |k, ev|
      [k, ev[0]]
    }.to_h  # Ruby 2.1 or later
  end
  private_class_method :_best_inherited_infinities


  # True if object is a type of Rangeary
  def self.is_rangeary_type?(obj)
    obj.respond_to?(:infinities) && obj.class.method_defined?(:first_element)
  end
  private_class_method :is_rangeary_type?

end # class Rangeary < Array

# Overwrites its equal operator
class Array
  alias_method :equals_before_rangeary, :== if ! self.method_defined?(:equals_before_rangeary)

  # Updated Array#==
  #
  # This modifies the original equality operator so that it returns true
  # when both self and other are *practically* empty,
  # that is, if +#empty_element?+ and/or +empty?+ are true for both, they are equal.
  #
  # This modification is necessary because {Rangeary#empty?} never returns +true+;
  # when it has no Range elements, it holds +RangeExtd::NONE+ or equivalent,
  # in whih case {Rangeary#empty_element?} returns +true+.
  #
  # @param other [Object]
  def ==(other)
    return true  if equals_before_rangeary other
    #return false if !other.class.method_defined?(:to_ary)
    #return false if !self.class.method_defined?(:empty_element?) && !other.class.method_defined?(:empty_element?)

    # It was false.  Is it?
    # eg., (Rangeary[RangeExtd::NONE] == []) is true,
    # because Rangeary[] with zero components does not exist!
    # Now either other or self is guranteed to be Rangeary.
    self_empt  = (respond_to?(:empty_element?) ? empty_element? : empty?)
    other_empt = (other.respond_to?(:empty_element?) ? other.empty_element? : other.empty?)
    self_empt && other_empt

    #return true  if self_empt && other_empt
    #return false if self_empt ^  other_empt
    ## return false if size != other.size  # evaluated at the beginning.
    #return false if size >= 2 && self[0..-2] != other[0..-2]
    #return false if !self[-1].respond_to?(:exclude_end?) || !other[-1].respond_to?(:exclude_end?)

    ## Now, both are guaranteed to have the same number of non-zero elements,
    ## all their elements except for the last one are equal,
    ## and their last elements are Range-type instances.
    ## Yet, the standard "equal" operator has failed.
    ## Only the potential they may be equal is their last elements differ
    ## between Endless Range (Ruby 2.6) and Infinity.
    #c_self  = Rangeary.comparable_end self[-1]
    #c_other = Rangeary.comparable_end other[-1]
    #return false if c_self != c_other

    #if !c_self.class.method_defined?(:infinite?) && !c_self.class.method_defined?(:infinity?)
    #  # :infinite? for Float::INFINITY, :infinity? is defined in RangeExtd::Infinity
    #  warn "sanity check failed. c_self should be infinite (#{c_self.inspect}). The result is not guranteed. Contact the code developer."
    #end

    ## The end of their last elements are both positive Infinity.
    ## How about "begin"?
    #self_flag  = (self[-1].class.method_defined?(:exclude_begin?)  ? self[-1].exclude_begin?  : false)
    #other_flag = (other[-1].class.method_defined?(:exclude_begin?) ? other[-1].exclude_begin? : false)
    #(self[-1].begin == other[-1].begin) && (self_flag == other_flag)
  end
end  # class Array

