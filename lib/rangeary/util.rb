# -*- coding: utf-8 -*-

class Rangeary < Array
  # Utility private methods for internal use for both Rangeary instance and class methods.
  #
  # External RangeExtd (range_extd) gem must be required.
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

    # Module to extend a Hash class instance to hold infinities information
    #
    # {Rangeary} needs to know what its own infinities are.
    # This class holds the information.
    #
    # It should have only 2 keys of +:negative+ and +:positive+ though it is
    # not constrained at the class level.
    #
    # In addition, method {HashInf#status} returns a Hash containing status
    # information about the main values (negative and positive infinities).
    #
    # The default infinity values (for negative and positive) are +false+
    # (NOT nil because nil can mean the border of a beginless/endless Range)
    # and default (uninitialized) status is nil.
    #
    module HashInf
      # Order Array of the {#status} parameters (smaller means a higher priority).
      # If +:definite+, it means it exists in a Range/Rangeary or is specified in an argument.
      STATUS_ORDERS = [:definite, :guessed, nil]

      # Reverse Hash of {STATUS_ORDERS} to give the priority number for {#status}.
      #
      # @example
      #  STATUS_PRIORITIES[:definite]  # => 0 
      #  STATUS_PRIORITIES[nil]        # => 2
      STATUS_PRIORITIES = STATUS_ORDERS.each_with_index.to_a.reverse.to_h

      # Method to overwrite Hash @status . See {#status}.
      attr_writer :status

      # Altenative constructor
      #
      # Usually you can construct an instance like:
      #   myhs = { negative: false, positive: false }
      #   myhs.extend HashInf
      #   myhs.status = { negative: negative, positive: positive }
      #
      # This constructor is a shortcut.
      #
      # Note that the returned object is a different object from the argument,
      # and so you are free to destructively modify it.
      #
      # If the argument is a HashInf, the contents are copied.
      #
      # @example
      #   HashInf.construct()  # => <HashInf: {negative: false, positive: false}, status: {negative: nil, positive: nil}>
      #   HashInf.construct({negative: false, positive: false}, status: {negative: :guessed})
      #
      # @param infinities [Hash, NilClass] if given, make sure the Hash has 2 keys of :negative and :positive with relevant infinity values, e.g., +Float::INFINITY+ or nil or else.
      # @param status [Hash] 1 or 2 keys of :negative and :positive with values of {HashInf::STATUS_ORDERS}. 
      # @return [Hash] extended with {HashInf}
      def self.construct(infinities=nil, status: {})
        rethsinf = {negative: false, positive: false}
        rethsinf.merge!(infinities) if infinities
        rethsinf.extend HashInf
        rethsinf.status = {negative: nil, positive: nil}.merge(status)
        rethsinf.status.merge!(infinities.status) if infinities.respond_to? :status
        rethsinf.status.merge!(status)
        rethsinf
      end

      # Reader and initializer of @status
      #
      # status is a two-key (+:negative+ and +:positive+) Hash with values of one of
      #
      # 1. nil (initial condition)
      # 2. +:guessed+  (if it is guessed, such as +Float::INFINITY+ because Range contains an Integer.)
      # 3. +:definite+  (if it is user-specified or exists in a Range, such as +nil+ in +(5..nil)+.
      def status
        return @status if @status
        @status = {negative: nil, positive: nil}
      end

      # merge another HashInf, taking into account the status
      #
      # If the statuses (HashInf#status) are the same, the more extreme
      # value is adopted.  Therefore,
      #       rang = Rangeary(?a..?c, positive: ?z)
      #   r = rang + Rangeary(?c..?f, positive: ?y)
      #   r.infinities[:positive]  # => ?z  (NOT ?z)
      #
      # If the old and new infinities are incomparable, the result is uncertain.
      #
      # @param hsinf [HashInf, Hash] if it is Hash, converted into HashInf.
      # @param force [Boolean] if true, other always has a higher priority like +Hash#merge+
      #    unless the status is false, in which case nothing changes.
      #    This is useful to handle the parameters given by the direct arguments to {Rangeary.new}
      # @return [HashInf]
      def merge_hashinf(other, force: false)
        rethsinf = HashInf.construct(self)
        rethsinf.merge_hashinf!(other, force: force)
        rethsinf
      end

      # Destructive version of {#merge_hashinf}
      #
      # @param (see #posinega_with_status=)
      # @return [HashInf]
      def merge_hashinf!(other, force: false)
        if !other.respond_to? :status
          other = HashInf.construct(other)
        end

        POSNEG2METHOD.each_key do |posneg|
          cmped = (force ? 1 : (STATUS_PRIORITIES[self.status[posneg]] <=> STATUS_PRIORITIES[other.status[posneg]]))
#print "DEBUG:408:mergeh: [force, cmped, posneg, self, other]=";p [force, cmped, posneg, self, other].inspect
          case cmped
          when -1
            # Do nothing
          when 0, 1
            next if false == other[posneg]  # no change if the status of "other" is false; n.b., when force==true, chances are self[posneg]!=false and other[posneg]==false, because "cmped" for the *case* is modified!
            if 0 == cmped
#print "DEBUG:409:mergeh: [force, posneg, self[p], other[p]]=";p [force, posneg, self[posneg], other[posneg]].inspect
              if false != self[posneg]
                oper = ((posneg == :negative) ? :< : :>)
#begin
                to_change = _former_more_extreme?(oper, self[posneg], other[posneg])
#rescue
#  print "DEBUG:412:mergeh: [force, cmped, posneg, self, other]=";p [force, cmped, posneg, self, other].inspect
#  raise
#end
                next if !to_change
              end
            end
            set_posinega_with_status(posneg, other[posneg], (force ? :definite : other.status[posneg]))
          else
            raise "should not happen. Contact the code developer."
          end
        end

        self
      end

      # Returns true if the former is more extreme.
      #
      # If +v1+ is +nil+, returns +true+.
      # If not and if +v2+ is +nil+, returns +false+.
      #
      # The following fails because whereas the guessed positive infinity
      # from the first one is Float:INFINITY, the second given infinity
      # is +RangeExtd::Infinity::POSITIVE+, which is not comparable
      # with +Float:INFINITY+ (although it is comparable with general Float).
      #
      #   Rangeary(..6, 5..RangeExtd::Infinity::POSITIVE)
      #
      # @param oper [Symbol] Method of operation: +return v1 if v1 oper v2+
      # @param v1 [Object]
      # @param v2 [Object]
      # @raise [ArgumentError]
      def _former_more_extreme?(oper, v1, v2)
        return true if v1.nil?
        return false if v2.nil?
        v1.send(oper, v2)
      rescue ArgumentError => err
        raise ArgumentError, "Inconsistent given infinities: "+err.message
      end
      private :_former_more_extreme?

      # Recommended way to set (update) the positive infinity with a status with verification
      #
      # @param (see #posinega_with_status=)
      # @return [self]
      def set_positive_with_status(infinity, stat)
        set_posinega_with_status(:positive, infinity, stat)
      end

      # Recommended way to set (update) the negaitive infinity with a status with verification
      #
      # @param (see #posinega_with_status=)
      # @return [self]
      def set_negative_with_status(infinity, stat)
        set_posinega_with_status(:negative, infinity, stat)
      end

      # Recommended way to set (update) the negaitive infinity with a status with verification
      #
      # @param infinity [Object]
      # @param stat [Symbol, NilClass] one of {HashInf::STATUS_ORDERS}. 
      # @return [self]
      def set_posinega_with_status(posneg, infinity, stat)
        self[posneg] = infinity
        raise ArgumentError, "stat must be one of #{STATUS_ORDERS.inspect}" if !STATUS_ORDERS.include? stat
        self.status[posneg] = stat
        self
      end

      # Return true if the specified infinity is +definite+
      #
      # @param posneg [Symbol] either :positive or :negative
      def definite?(posneg)
        status_is_a?(:definite, posneg)
      end

      # Return true if the specified infinity is +guessed+
      #
      # @param posneg [Symbol] either :positive or :negative
      def guessed?(posneg)
        status_is_a?(:guessed, posneg)
      end

      # Return true if the specified infinity is with a specified status.
      #
      # @param stat [Symbol, NilClass] One of the elements of {STATUS_ORDERS}
      # @param posneg [Symbol] either :positive or :negative
      def status_is_a?(stat, posneg)
        raise ArgumentError, "Invalid specified status of #{stat.inspect}" if !STATUS_ORDERS.include? stat
        case posneg
        when :positive, :negative
          stat == status[posneg]
        else
          raise ArgumentError, "Invalid argument, neither :positive nor :negative: #{posneg.inspect}"
        end
      end


      # Modify +Hash#inspect+ (irb does not take this into account)
      #
      # @return [String]
      def inspect
        sprintf "<Hash(Inf): %s, status: %s>", super, status
      end
    end # module HashInf

    ########################################

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

      Rangeary.flatten_no_rangeary(arran).each do |rang|  # Range, RangeExtd, Rangeary
        _validate_opts_infinities([rang], infs: reths)
        reths.merge_hashinf!(_get_infinities_from_obj(rang))
      end
      reths
    end
    private :_get_infinities_from_multiple


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
#print "DEBUG563:from_ran: method,val=";p [metho,val]
        inf, stat = _get_infinity_status_single(val, posneg)
        reths.set_posinega_with_status(posneg, inf, stat)
      end
#print "DEBUG568:from_ran: reths=";p reths

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
    # Note that if the infinities at both ends are a user specifies
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
#print "DEBUG362:adjusted: original reths=";p [reths]

      # Sets the priority Hash (not HashInf).
      hspri = {}
      reths.each_pair do |posneg, inf|
        hspri[posneg] =
          PRIORITIES.each_with_index do |klass, i|
            break i if inf.is_a?(klass)
          end
        raise "Should not happen" if !hspri[posneg]
      end
#print "DEBUG363:adjusted: reths,hspri=";p [reths,hspri]
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
#print "DEBUG364:adjusted: reths=";p [reths]
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
#print "DEBUG365:adjusted: reths=";p [reths]
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
  def _validate_select_infinities(ar_inherit, hs_opts=nil)
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
    if $VERBOSE
      hs_inherit.each_pair do |ek, ev|  # loop over [:positive, :negative]
        ev_uniq = ([hs_opts[ek]]+ev).compact.uniq
        ev_uniq.delete false
        msg = "Inconsistent %s infinities are found: %s (=> %s is used)"%[ek, ev_uniq.inspect, hsret[ek]]
        #warn msg if $VERBOSE && (ev_uniq.size > 2) && (!ev_uniq.all?{ |c| RangeExtd::Infinity.infinite?(c) })
        warn msg if (ev_uniq.size > 1) && (!ev_uniq.all?{ |c| RangeExtd::Infinity.infinite?(c) })
warn "DEBUG:998:warn: hs_inherit=#{hs_inherit}, hs_opts=#{hs_opts}" if (ev_uniq.size > 1) && (!ev_uniq.all?{ |c| RangeExtd::Infinity.infinite?(c) }) && Float === ev_uniq[-1]       ################
warn "DEBUG:999:warn: #{ev_uniq.map(&:class).map(&:inspect)}" if (ev_uniq.size > 1) && (!ev_uniq.all?{ |c| RangeExtd::Infinity.infinite?(c) })      ################
      end
    end

    hsret
  end
  private :_validate_select_infinities

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

    # True if object is a type of Numeric and comparable
    def is_num_type?(obj)
      Numeric === obj && obj.respond_to?(:between?) && obj.class.method_defined?(:<)
    end
    private :is_num_type?

  end # module Util
end   # class Rangeary < Array

