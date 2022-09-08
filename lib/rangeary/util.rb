# -*- coding: utf-8 -*-

require "range_extd" if !defined? RangeExtd
require_relative "util/hash_inf.rb"

class Rangeary < Array
  # Utility containing {HashInf} module and some private methods for internal use for both Rangeary instance and class methods.
  #
  # External RangeExtd (range_extd) gem must be required.
  # This module is supposed to be used in combination with {Rangeary}.
  #
  # Many methods have been migrated from +rangeary.rb+ so that this module
  # is self-consistent, i.e, this module does not call Rangeary-specific
  # methods except for {Rangeary.initialize}.
  #
  # All methods (but the module {HashInf}) in this module are both
  # included and extended by {Rangeary}. All methods in this module
  # are private, because any public methods in this modules would be
  # exposed as both class and instance methods, which would be confusing to users.
  #
  # Some of the methods in this library are named +_core+, but they may be
  # in practice exactly what the original would be rather than only the core part
  # and the corresponding public method in the main file +rangeary.rb+ does
  # no more than simply calls it.  The reason of the structure is
  # simply because this module is both included and extended (a bit tricky combination)!
  module Util

    # private constants: correspondance to keywords to Range methods
    POSNEG2METHOD = { negative: :begin, positive: :end }
    private_constant :POSNEG2METHOD

    # private constants: Reverse order of the classes in guessing implicit infinities.
    PRIORITIES = [FalseClass, NilClass, Numeric, RangeExtd::Infinity, Object]
    private_constant :PRIORITIES

    # private constants: Positive implicit infinities in the reverse order.
    PRIORI_OBJ = [false, nil, Float::INFINITY, RangeExtd::Infinity::POSITIVE, Object.new] # the last one is meaningless!
    # NOTE: if you change the order, make sure to ajust the numbers in _get_adjusted_infinities().
    private_constant :PRIORI_OBJ

    ####################
    private
    ####################

    ## Returns infinities literally included in the given Range
    ##
    ## @param ran [Rangeary, Range]
    ## @return [Hash<Object>] 2 keys of :positive and :negative. value of false if not found anything.
    #def _definite_infinities_from_range(ran)
    #   return HashInf.construct if ran.is_none?  # RangeExtd::NONE

    #   hs =
    #     HashInf.construct.map{|k, v|
    #       [k, ((v.respond_to?(:infinite?) && v.infinite?) ? v : nil)]
    #     }.to_h  # This is a standard Hash (NOT HashInf-extended)

    #   stats = hs.map{|k, v| [k, ((false == v) ? nil : :definite)]}.to_h
    #
    #   HashInf.construct(hs, status: stats)
    #end
    #private :_definite_infinities_from_range

    # Get an infinity and status from a single value
    #
    # The returned +status+ is either of the first two {HashInf::STATUS_ORDERS}
    # that is, excluding +nil+; +status+ is never +nil+ because the infinity is
    # always guessable for a "valid" Range, i.e., Range with all elements mutually
    # comparable, except for +RangeExtd::NONE+ .
    # (The caller must judge about +RangeExtd::NONE+)
    #
    # The result will be fed to {HashInf#set_posinega_with_status}.
    #
    # This routine does not (and cannot) check the validity of the infinity,
    # e.g., the positive infinity may be -Float::INFINITY .
    #
    # @param val [Object]  nil, if it is a begin/end of a Borderless Range.
    # @param posneg [Symbol]  Either :negative or :positive (infinity)
    # @return [Array<Object, Symbol>] 2-element Array of [infinity, :definite|:guessed]
    def _get_infinity_status_single(val, posneg)
      # Two cases of :definite
      if val.nil?
        return [nil, :definite]
      elsif val.respond_to?(:infinite?) && val.infinite?
        return [val, :definite]
      end

      # The status is now :guessed
      inf =
        case val
        when RangeExtd::Infinity
          val
        when Numeric
          ((:negative == posneg) ? -1 : 1) * Float::INFINITY
        else
          nil
        end
      [inf, :guessed]
    end
    private :_get_infinity_status_single


    # Returns infinities taken or guessed from multiple Range-s and Rangeary-s
    #
    # See {#_get_infinities_from_obj} for detail.
    #
    # @param arran [Array<Rangeary, Range>]
    # @return [Hash<Object>] (HashInf) 2 keys of :positive and :negative.
    def _get_infinities_from_multiple(*arran)
      reths = HashInf.construct

      flatten_no_rangeary_core(arran).each do |rang|  # Range, RangeExtd, Rangeary
        _validate_opts_infinities([rang], infs: reths)
        reths.merge_hashinf!(_get_infinities_from_obj(rang))
      end
      reths
    end
    private :_get_infinities_from_multiple


    # Core routine for {Rangeary.flatten_no_rangeary}
    #
    # @param arin [Array<Range, RangeExtd, Rangeary, Array>]
    # @return [Array<Range, RangeExtd, Rangeary>]
    def flatten_no_rangeary_core(*arin)
      arret = []
      arin.each do |val|
        if val.respond_to?(:infinities) || val.respond_to?(:exclude_end?)
          arret << val
        elsif val.respond_to?(:flatten)
          arret.concat send(__method__, *val)
        else
          raise ArgumentError, "Argument Array must consist of only Range (RangeExtd) or Array (Rangeary)"
        end
      end
      arret
    end
    private :flatten_no_rangeary_core

    # Returns infinities taken or guessed from the given Range or Rangeary
    #
    # If the given Range has an infinity (judged with [#infinity?])
    # at its either end, it is respected, and +nil+ is the same.
    # Otherwise, guessed from the elements.
    #
    # The returned Hash extended with HashInf module consists of
    # two keys of +:positive+ and +:negative+ for respective infinities.
    # Also, it has the method {HashInf#definite?}, {HashInf#guessed?},
    # {HashInf#status} and alike to represent the degree of confidence.
    #
    # @param ran [Rangeary, Range]
    # @return [Hash<Object>] (HashInf) 2 keys of :positive and :negative.
    def _get_infinities_from_obj(ran)
      return _get_infinities_from_range(ran)          if ran.respond_to? :exclude_end?
      return ran.instance_variable_get("@infinities") if ran.respond_to? :infinities
        # to get the instance variable @infinities, as opposed to Rangeary#infinities
      raise ArgumentError, "Neither Range/RangeExtd nor Rangeary is given."
    end
    private :_get_infinities_from_obj

    # Returns infinities taken or guessed from elements of the given Range
    #
    # If the given Range has an infinity (judged with [#infinity?])
    # at its either end, it is respected, and +nil+ is the same.
    # Otherwise, guessed from the elements.
    #
    # The returned Hash extended with HashInf module consists of
    # two keys of +:positive+ and +:negative+ for respective infinities.
    # Also, it has the method {HashInf#definite?}, {HashInf#guessed?},
    # {HashInf#status} and alike to represent the degree of confidence.
    #
    # @param ran [Rangeary, Range]
    # @return [Hash<Object>] (HashInf) 2 keys of :positive and :negative.
    def _get_infinities_from_range(ran)
      reths = HashInf.construct
      return reths if ran.is_none?  # RangeExtd::NONE

      POSNEG2METHOD.each_pair do |posneg, metho|  # n.b. method() is a Ruby built-in function.
        val = ran.send(metho)
        inf, stat = _get_infinity_status_single(val, posneg)
        reths.set_posinega_with_status(posneg, inf, stat)
      end

      _get_adjusted_infinities(reths)
    end
    private :_get_infinities_from_range

    # Returns adjusted infinities
    #
    # "adjusted" means:
    # for example, when a Range of +("b"..RangeExtd::Infinity::POSITIVE)+ is given,
    # the standard guess of infinities (with #{_get_infinity_status_single})
    # are +(nil..RangeExtd::Infinity::POSITIVE)+.
    # However, the two infinities are inconsistent and it is better
    # to be +(RangeExtd::Infinity::NEGATIVE..RangeExtd::Infinity::POSITIVE)+
    # given that the user deliberately set +RangeExtd::Infinity::POSITIVE+.
    # In such a case, the tentative negative infinity "nil" is replaced with
    # +RangeExtd::Infinity::NEGATIVE+ to make everything consistent.
    #
    # In short, this method looks into the infinities at both ends of a Range,
    # validates them, and corrects them if need be.
    #
    # Note that if the infinities at both ends are a user specifies they are respected
    # unless both infinites have the same polarity, in which case the initialized
    # default infinities {HashInf} is returned.
    #
    # Priorities are in the standard order (though {Util::PRIORITIES} is in the reverse order):
    #
    # 1. exact value
    # 2. RangeExtd::Infinity
    # 3. Float::INFINITY
    # 4. nil
    # 5. false
    #
    # This order basically follows the order of unlikeliness of what a user does.
    # For example, +RangeExtd::Infinity+ cannot never be set unless a user deliberately sets it.
    # Therefore, if a user does it, it should be respected. But if a user doesn't,
    # other infinities must be used (in Ruby-2.7).  Note +false+ is the initial state.
    #
    # Note that if an exact value is given, unless the other end is ":definite",
    # nil is set.
    #
    # @param ininf [Hash] 2 keys of :positive and :negative.
    # @return [Hash<Object>] (HashInf) 2 keys of :positive and :negative. false if not found anything.
    def _get_adjusted_infinities(ininf)
      reths = HashInf.construct(ininf)

      # If both infinites have the same polarity, the information is reset.
      return HashInf.construct() if _infinites_with_same_polarity?(*(ininf.values))

      # Sets the priority Hash (not HashInf).
      hspri = {}
      reths.each_pair do |posneg, inf|
        hspri[posneg] =
          PRIORITIES.each_with_index do |klass, i|
            break i if inf.is_a?(klass)
          end
        raise "Should not happen" if !hspri[posneg]
      end
      return reths if hspri[:positive] == hspri[:negative]

      if hspri[:positive] < hspri[:negative]
        # :negative has a higher priority

        # This might happen when
        # the Infinity at an end is a user-specified one (Object) and the other end
        # is RangeExtd::Infinity or Float::INFINITY
        return reths if reths.definite?(:positive)

        inf =
          case hspri[:negative]
          when 2, 3
            PRIORI_OBJ[hspri[:negative]]  # either RangeExtd::Infinity::POSITIVE or Float::INFINITY
          when 4
            # user-defined infinite object is taken into account.
            (reths[:positive].respond_to?(:infinite?) && reths[:positive].infinite?) ? reths[:positive] : nil
          else  # should be 1 only ((false..false) may be an exception, though invalid for RangeExted or Rangeary?)
            nil
          end
        reths.set_positive_with_status(inf, :guessed)
        return reths
      end

      return reths if reths.definite?(:negative)  # Very unlikely, but it might happen (see above).

      # Now, negative infinity is overwritten.
      begin  # "begin" just for indentation.
        inf =
          case hspri[:positive]
          when 2, 3
            -PRIORI_OBJ[hspri[:positive]] # either RangeExtd::Infinity::NEGATIVE or -Float::INFINITY
          when 4
            # user-defined infinite object is taken into account.
            (reths[:negative].respond_to?(:infinite?) && reths[:negative].infinite?) ? reths[:negative] : nil
          else
            nil
          end
      end
      reths.set_negative_with_status(inf, :guessed)

      reths
    end
    private :_get_adjusted_infinities

    # Validate @infinities from the command-line options.
    #
    # This basically applies +<=>+ operator and if an Exception raises,
    # converts it to ArgumentError.
    # Almost no Object should fail, because +<=>+ is defined in
    # the Object class and it does not fail!  For example, +3 <=> "a"+
    # returns +nil+ and so it does not fail.
    #
    # @param arin [Array<Range, RangeExtd, Rangeary>]
    # @param infs [Hash]
    # @return [void]
    # @raise [ArgumentError]
    def _validate_opts_infinities(arin, infs: @infinities)
      infs.each_pair do |ek, my_inf|
        next if !my_inf #|| RangeExtd::Infinity.infinite?(my_inf)
        arin.flatten.each do |er|  # Rangeary is flattened.
          next if er.is_none?  # Required for Ruby 2.7+ (for updated RangeExtd::NONE for beginless Range)
          [er.begin, er.end].each do |ev|
            next if !ev
            next if (is_num_type?(ev) && is_num_type?(my_inf))
            begin
              case my_inf <=> ev
              when -1, 0, 1
                next
              else # nil or ralse
                next
              end
            rescue
              msg = "invalid parameter for :#{ek} => (#{my_inf.inspect}), incompatible with the range with Range=(#{er.inspect})."
              raise ArgumentError, msg
            end
          end
        end
      end
    end
    private :_validate_opts_infinities


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
    def _get_cand_infinities(arin)
      hsret = { positive: [], negative: [] }
      arin.each do |ec|
        hsret.each_key do |k|
          hsret[k] << ec[k]
        end
      end
      hsret
    end
    private :_get_cand_infinities

    # Sort infinities obtained from inherited objects
    #
    # RangeExtd::Infinity is ignored.  Float::INFINITY has the lowest priority.
    #
    # @param hs_inherit [Hash<Array>] :positive => Array, etc (From Rangeary-s in the main Array given)
    # @return [Hash<Array>] each key (:(posi|nega)tive) contains the sorted candidate Array.
    def _sort_inherited_infinities_all(hs_inherit)
      hs_inherit.map do |ek, ev|
        [ek, _sort_inherited_infinities_each(ev, key=ek)]
      end.to_h  # Ruby 2.1 or later
    end
    private :_sort_inherited_infinities_all

    # Sort infinities obtained from inherited objects
    #
    # RangeExtd::Infinity is ignored.  Float::INFINITY has the lowest priority.
    #
    # @param ar_infs [Array] of Infinities (inherited)
    # @param key [Symbol] :positive or :negative
    # @return [Array]
    def _sort_inherited_infinities_each(ar_infs, key=:positive)
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
    private :_sort_inherited_infinities_each

    # True if two values are both infinite and have the same polarity
    #
    # Both are assumed to have methods of :positive? and :negative?,
    # if they have a method of +infinite?+ (as +Float::INFINITY+ does).
    #
    # @param v1 [Array]
    # @param v2 [Array]
    def _infinites_with_same_polarity?(v1, v2)
      arran = [v1, v2]
      (( arran.all?{ |ea| RangeExtd::Infinity.infinite?(ea) } &&
         (arran.all?(&:positive?) || arran.all?(&:negative?)) ))
       #(arran.all?(&:nil?))) # allowed in Ruby 2.7+
    end
    private :_infinites_with_same_polarity?


    # Core routine for {Rangeary.sort_ranges}
    #
    # @param ar [<Range, RangeExtd>] Arbitrary number.
    # @return [Array<Range, RangeExtd>]
    def sort_ranges_core(*ar)
      ar.flatten.sort{ |a,b|
        err_msg_ab = "invalid parameter (#{a.inspect} or #{b.inspect})."

        if a.is_none?
          next (b.is_none? ? 0 : -1)
        elsif b.is_none?
          next 1
        end

        # Since Ruby-2.6, the end can be nil (Endless Range).
        # Before Ruby-2.6, they used to raise Exception (ArgumentError) in Range
        # and so they would not have sneaked in to RangeExtd, either.
        # The following is in that sense meaningful only for Ruby 2.6 and later.
        #
        # Even in Ruby-2.7, this routine need no change, because nil is
        # correctly handled.
        ends = { :a => a, :b => b }
        ends.each_pair do |k, v|
          ends[k] = comparable_beginend_core(v).end
        end

        # ret = begs[:a] <=> begs[:b]
        ret = a.begin <=> b.begin  # Without filtering with comparable_beginend()
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
    end	# def sort_ranges_core(ar)
    private :sort_ranges_core


    # Core routine of {Rangeary.comparable_beginend}
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
    def comparable_beginend_core(ran, vend=Object, infinities: nil)
      if ran.respond_to? :is_none?
        if ran.is_none?  # Range should have the method.
          return RangeExtd::NONE
        end
        _ = RangeExtd(ran)         # may raise RangeError
      else
        ran = RangeExtd(ran, vend) # may raise RangeError
      end

      ar_inf = [infinities[:negative], infinities[:positive]] if infinities

      is_numeric = [:begin, :end].any?{|method| ran.send(method).respond_to? :to_int}
      arret = [:begin, :end].map.with_index{ |method, i|
        obj = ran.send(method)
        if obj.nil?
          if infinities && ar_inf[i]  # infinity of nil or false is ignored.
            ar_inf[i]
          else
            # plus-minus is reversed only for "begin", like RangeExtd::Infinity::NEGATIVE
            val = (is_numeric ? Float::INFINITY : RangeExtd::Infinity::POSITIVE)
            (i == 0) ? -val : val
          end
        else
          obj
        end
      }
      RangeExtd(arret[0], arret[1], (ran.respond_to?(:exclude_begin?) ? ran.exclude_begin? : false), ran.exclude_end?)
    end
    private :comparable_beginend_core

    # Internal version of Conjunction, where +infinities+ can be given.
    #
    # @param r1 [Rangeary, RangeExtd, Range]
    # @param r2 [Rangeary, RangeExtd, Range]
    # @param infs [Hash] ({HashInf}) if supplied, +infinities+ are NOT guessed from the main arguments.  Note that there is an instance method called +infinities+()
    # @return [Rangeary]
    def conjunction_core(r1, r2, infs: nil)

      r1 = Rangeary.new(r1) if ! defined? r1.first_element
      r2 = Rangeary.new(r2) if ! defined? r2.first_element

      hsinf = (infs || _get_infinities_from_multiple(r1, r2))
      return Rangeary.new(RangeExtd::NONE, **hsinf) if r1.null? || r2.null?
      #return Rangeary.new(r1, **hsinf) if r2.all?
      #return Rangeary.new(r2, **hsinf) if r1.all?
      return(infs ? Rangeary.new(r1, infs) : Rangeary.new(r1, **hsinf)) if r2.all?
      return(infs ? Rangeary.new(r2, infs) : Rangeary.new(r2, **hsinf)) if r1.all?

      # Initialisation
      a1 = r1.to_a
      a2 = r2.to_a
      rc = Rangeary.new( RangeExtd::NONE, **hsinf )  # hsinf is essential to define @infinities

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
          #
          # Now, considering Ruby-2.7 Beginless Range, too.
          ran1 = comparable_beginend_core(ea1)
          ran2 = comparable_beginend_core(ea2)
          break if ran2.end < ran1.begin	# Completely out of range
          if ran1.end < ran2.begin	# Completely out of range
            last_a1index = ind if last_a1index < ind
            next
          end

          # Core - Perform conjunction.
          pq1 = conjunctionRangeExtd(ea1, ea2)	# => Rangeary.conjunctionRangeExtd()
          if ! pq1.empty?
            rc += (infs ? Rangeary.new(pq1, infs) : Rangeary.new(pq1))
            last_a1index = ind
          end
        end	# a1.each_with_index do |ea1, ind|
      end		# a2.each do |ea2|

      rc
    end	# def self.conjunction_core(r1, r2)
    private :conjunction_core


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
    def conjunctionRangeExtd(r1, r2)

      [r1, r2].each do |er|
        return er if er.is_none?
      end

      r = *( sort_ranges_core([RangeExtd(r1), RangeExtd(r2)]) )	# => Rangeary.sort_ranges

      ## Note: the end product will be (cBeg(:stBeg), cEnd(:stEnd))
      #    where :stBeg and :stEnd mean exclude_(begin|end)?

      # Set the candidate begin value.
      cBeg  = r[1].begin
      if r[0].begin == r[1].begin
        stBeg = (r[1].exclude_begin? || r[0].exclude_begin?)
      else
        stBeg =  r[1].exclude_begin?
      end

      # Set the candidate end value.  (Rangeary.comparable_end() for Ruby-2.6 Endless Range)
      # Note: this comparison ignores @infinities even if set,
      #   because @infinities may not be defined in the arguments!
      #   Anyway, nothing should be larger than the upper limit
      #   and so this should be fine.
      if comparable_beginend_core(r[0]).end == comparable_beginend_core(r[1]).end
        cEndOrig = r[1].end
        cEnd     = comparable_beginend_core(r[1]).end
        stEnd = (r[0].exclude_end? || r[1].exclude_end?)
      else
        a = [[comparable_beginend_core(r[0]).end, 0, r[0].end], [comparable_beginend_core(r[1]).end, 1, r[1].end]].min
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
    private :conjunctionRangeExtd

    # True if object is a type of Numeric and comparable
    def is_num_type?(obj)
      Numeric === obj && obj.respond_to?(:between?) && obj.class.method_defined?(:<)
    end
    private :is_num_type?

  end # module Util
end   # class Rangeary < Array

