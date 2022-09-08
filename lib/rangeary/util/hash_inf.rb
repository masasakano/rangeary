# -*- coding: utf-8 -*-

class Rangeary < Array
  module Util

    # Module to extend a Hash class instance to hold infinities information
    #
    # {Rangeary} needs to know what its own infinities are.
    # This class holds the information.
    #
    # It should have only 2 keys of +:negative+ and +:positive+ though it is
    # not constrained at the class level.
    #
    # Method {HashInf#status} returns a Hash containing status
    # information about the main values (negative and positive infinities).
    # Some related utility methods are also defined.
    #
    # The default infinity values (for negative and positive) are +false+
    # (NOT +nil+ because nil can mean the border of a beginless/endless Range),
    # whereas the default (uninitialized) status for it is +nil+.
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
      # If the argument is a {HashInf}, the contents are copied.
      #
      # @example
      #   HashInf.construct()  # => <HashInf: {negative: false, positive: false}, status: {negative: nil, positive: nil}>
      #   HashInf.construct({negative: false, positive: false}, status: {negative: :guessed})
      #
      # @param infinities [Hash, NilClass] if given, make sure the Hash has 2 keys of :negative and :positive with relevant infinity values, e.g., +Float::INFINITY+ or nil or else.
      # @param status [Hash] Optional. 1 or 2 keys of :negative and :positive with values of {HashInf::STATUS_ORDERS}.
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

      # merges another {HashInf} (or Hash) like +Hash#merge+, taking into account the status, and returns a new {HashInf}
      #
      # If other is a (non-HashInf) Hash, it should have no more than 2 keys of :positive and :negative.
      #
      # If the statuses {HashInf#status} are the same between self and other, the more extreme
      # value is adopted.  Therefore,
      #       rang = Rangeary(?a..?c, positive: ?z)
      #   r = rang + Rangeary(?c..?f, positive: ?y)
      #   r.infinities[:positive]  # => ?z  (NOT ?y)
      #
      # If the old and new infinities are incomparable, the result is uncertain.
      #
      # @param other [HashInf, Hash] if it is Hash, converted into HashInf.
      # @param force [Boolean] if true, other always has a higher priority like +Hash#merge+
      #    unless the corresponding status is +false+, in which case nothing changes.
      #    This is useful to handle the parameters given by the direct arguments to {Rangeary.initialize}
      # @return [HashInf, Hash]
      def merge_hashinf(other, force: false)
        rethsinf = HashInf.construct(self)
        rethsinf.merge_hashinf!(other, force: force)
        rethsinf
      end

      # Destructive version of {#merge_hashinf}
      #
      # @param (see #posinega_with_status=)
      # @return [HashInf, Hash]
      def merge_hashinf!(other, force: false)
        if !other.respond_to? :status
          other = HashInf.construct(other)
        end

        POSNEG2METHOD.each_key do |posneg|
          cmped = (force ? 1 : (STATUS_PRIORITIES[self.status[posneg]] <=> STATUS_PRIORITIES[other.status[posneg]]))
          case cmped
          when -1
            # Do nothing
          when 0, 1
            next if false == other[posneg]  # no change if the status of "other" is false; n.b., when force==true, chances are self[posneg]!=false and other[posneg]==false, because "cmped" for the *case* is modified!
            if 0 == cmped
              if false != self[posneg]
                oper = ((posneg == :negative) ? :> : :<)
                to_change = _former_more_extreme?(oper, self[posneg], other[posneg])
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

      # Recommended way to set (update) the negative infinity with a status with verification
      #
      # @param (see #posinega_with_status=)
      # @return [self]
      def set_negative_with_status(infinity, stat)
        set_posinega_with_status(:negative, infinity, stat)
      end

      # Recommended way to set (update) the infinity in either polarity with a status with verification
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

      # Return true if the infinity of the specified polarity is +:definite+
      #
      # @param posneg [Symbol] either :positive or :negative
      def definite?(posneg)
        status_is_a?(:definite, posneg)
      end

      # Return true if the infinity of the specified polarity is +:guessed+
      #
      # @param posneg [Symbol] either :positive or :negative
      def guessed?(posneg)
        status_is_a?(:guessed, posneg)
      end

      # Return true if the specified infinity is +nil+.
      #
      # @param posneg [Symbol] either :positive or :negative
      def status_is_nil?(posneg)
        status_is_a?(nil, posneg)
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


      # Alter +Hash#inspect+ (Note irb does not take this into account)
      #
      # @return [String]
      def inspect
        sprintf "<Hash(Inf): %s, status: %s>", super, status
      end
    end # module HashInf
  end # module Util
end   # class Rangeary < Array

