
# Rangeary - Multiple Range(Extd) class

This package defines Rangeary class, which contains multiple 1-dimensional
ranges ([RangeExtd](http://rubygems.org/gems/range_extd) objects).  For
example, a multiple range of

    0.5 < x <= 2.5,  6.0 <= x < 8.0,  10.0 <= x (<= Infinity)

for `x` can be defined in this class.

The element objects for `Rangeary` can be anything that can form RangeExtd
objects, not only Numeric (or Real) but anything Comparable.  `Rangeary`
accepts the built-in Range-class objects for the elements in initialization,
too, providing it is a valid (`Range#valid? == true`) ones for the
[RangeExtd](https://rubygems.org/gems/range_extd) class (n.b., most Range
objects are valid, but +(true..true)+ is not, for example, which would not
make sense to constitute multiple ranges).

All the four standard logical operations, that is, negation (`~`), conjunction
(`&` or `*`), disjunction (`|` or `+`) and exclusive disjunction (`^` or
`xor`) are defined, as well as subtraction (`-`).

{Rangeary} objects are immutable - once it is created, you cannot alter the
contents.

{Rangeary} is implemented as a sub-class of Array, works as an array of
RangeExtd, and inherits most of the methods. Thus, you can apply most
operations to {Rangeary} that Array accepts, within the restriction of
immutability of {Rangeary}; for example, +Array#push+ is disabled for
{Rangeary}. In addition, {Rangeary} offers several methods that directly work
on its element as a range.  In the example above, `#cover?(1.0)` (like
+Range#cover?+) returns true, whereas `#cover?(9.0)`, false.

With this class, logical operations of 1-dimensional range objects are now
possible and easy. I hope you find it to be useful.

### News: Library locations and support for Beginless Range

**IMPORTANT**: The path for the library is moved up by one directory in
{Rangeary} Ver.2 from Ver.1 (same as the change in `RangeExtd`) in order that
the location follows the Ruby Gems convention.  In short, the standard way to
require is +require "rangeary"+, the path of which used to be
"rangeary/rangeary"

Version of {Rangeary} is now 2.0.

Ruby 2.7 and later supports [Beginless
range](https://rubyreferences.github.io/rubychanges/2.7.html#beginless-range).

`Rangeary` (Ver.2) also supports it now, requiring
[RangeExtd](https://rubygems.org/gems/range_extd) Ver.2 (or later).

#### News: Endless Range supported

Now, as of 2019 October, this fully supports [Endless
Range](https://rubyreferences.github.io/rubychanges/2.6.html#endless-range-1)
introduced in Ruby 2.6.  It is released as Version 1.* finally!

## Install

First, you need [RangeExtd](https://rubygems.org/gems/range_extd) class
library Ver.2 or later, which is in gem:

    gem install range_extd

Or, get it from {https://rubygems.org/gems/range_extd}

If you choose manual installation for some reason, follow the INSTALL section
in the manual of [RangeExtd](https://rubygems.org/gems/range_extd)

Then, install this library with

    gem install rangeary

Files of

    rangeary.rb
    rangeary/util.rb
    rangeary/util/hash_inf.rb

should be installed in one of your `$LOAD_PATH`. Alternatively, get it from
{http://rubygems.org/gems/rangeary}

Then all you need to do is

    require 'rangeary'

in your Ruby script (or irb) (Note the path used to be, in Rangeary Ver.1 or
earlier), "rangeary/rangeary"), in which all the files are automatically
loaded.

{Rangeary} Ver.2, along with RangeExtd, works in only Ruby 2.7 or later.

Have fun!

## Simple Examples

### How to create a Rangeary instance

Here are some simple examples.

    r1 = RangeExtd(?a...?d, true) # => a<...d
    ra = Rangeary(?g..?h, r1)     # => [a<...d, g..h]
    Rangeary(RangeExtd::NONE)     # => [RangeExtd::NONE]
    Rangeary(RangeExtd::NONE, 1..5, 3...7)  # => [1...7]
    Rangeary(true..true)          # => ArgumentError

Basically, `Rangeary()` or `Rangeary.new()` accepts an arbitrary number of
either Range, RangeExtd, {Rangeary}, or a combination of them.  Note Range
objects that return false in +Range#valid?+ raise an exception.

For more detail and examples, see {Rangeary.initialize}.

### Practical application examples

    ra.to_a                       # => ["a"<..."d", "g".."h"]
    ra.cover?("a")                # => false
    ra.cover?("b")                # => true
    ra.end                        # => "h"
    ra.each do |i|
      print i.begin
    end    # => self ( "ag" => STDOUT )
    ra.each_element do |i|
      print i
    end    # => self ( "bcgh" => STDOUT )

    rb = Rangeary(6...9, 2..4)    # => [2..4, 6...9]
    rb + Rangeary(3..7)           # => [2...9]
    rb - Rangeary(3..7)           # => [2...3, RangeExtd(7,'<...',9)]
    rb * Rangeary(4..5, 8..10)    # => [4..4, 8...9]
    ~rb                           # => [-Float::INFINITY...2, RangeExtd(4,'<...',6), 9..Float::INFINITY]

    rc = Rangeary(?d...?x, negative: ?a, positive: ?z)
    ~rc                           # => [?a...?d, ?x..?z]

where +~rb+ is equivalent to `rb.negation`.  In the last example, it provides
the user-defined **infinities**.

Most of the methods that are in the built-in Array can be used, as long as it
does not violate the immutability of {Rangeary} objects, such as +Array#push+.

## Description

Once the file `rangeary.rb` is required, the class `Rangeary` is defined, in
addition to those defined in the
[RangeExtd](https://rubygems.org/gems/range_extd) library (`RangeExtd`,
`RangeExtd::Infinity`, and `RangeExtd::Nowhere`). `RangeExtd` adds some
methods to the built-in `Range` class.

### Rangeary Class

{Rangeary} objects are immutable, the same as Range and RangeExtd. Hence once
an instance is created, it would not change.

The ways to create an instance are explained above (in the Example sections). 
Any attempt to try to create an instance with one of elements being not
"valid" as a range, that is, of which +Range#valid?+ returns false, raises an
exception (`ArgumentError`), and fails.

`RangeExtd` (or `Range`) objects given as arguments to initialize `Rangeary`
instances may contain negative and/or positive infinity objects. Since Ruby
2.7 and 2.6, [Beginless
range](https://rubyreferences.github.io/rubychanges/2.7.html#beginless-range)
and [Endless
Range](https://rubyreferences.github.io/rubychanges/2.6.html#endless-range-1)
are respectively introduced. They correspond to more conventional
[RangeExtd::Infinity](https://www.rubydoc.info/gems/range_extd/RangeExtd/Infin
ity) objects, which Rangeary has supported since its first release.

The built-in borderless Ranges and `RangeExtd::Infinity` objects are similar
but are conceptually slightly different; the difference is similar to that
between borderless Ranges and `Float::INFINITY`. See the reference document of
[RangeExtd](http://rubygems.org/gems/range_extd) for detail.

In default, when Ranges that contain infinities, be it built-in borderless
Ranges or `Float::INFINITY` or `RangeExtd::Infinity` are given to {Rangeary},
`Rangeary` use them.  Alternatively, a user can specify their own infinity
objects; for example, you may provide +"a"+ as the negative infinity for
`Rangeary` with String Ranges.  If nothing is provided and yet if `Rangeary`
requires one(s), which may happen, for example, in an operation of negation,
`Rangeary` uses `Float::INFINITY` for Numeric (Real numbers) and `nil` for
anything else in default.  See {Rangeary} and {Rangeary.initialize} for
detail.

When multiple ranges are given in initializing, they are internally sorted in
storing, and if there are any overlaps among any of the elements, they are
treated as disjunction (that is, simple summation).  This means that the
objects a {Rangeary} instance holds internally can be different from  the
objects given in initialization, namely, their `#object_id` may be different. 
In particular, if built-in Range objects are given, they are always converted
into RangeExtd objects internally.

If any of the given range in initialization is "empty", that is,
+Range#empty?+ returns true, they are ignored, unless all of the ranges given
are empty ranges, in which case the "smallest" one will be preserved.

If the result of the operation is empty, only the element of the resultant
{Rangeary} is `RangeExtd::NONE`, and hence {Rangeary#empty_element?} will
return true.

For any Rangeary objects, `(Rangeary#to_a).size` returns always positive and
not zero for this reason, or +Rangeary#empty?+ returns always false, as
+Rangeary#empty?+ is a method inherited from Array. Use
{Rangeary#empty_element?} instead to check whether the instance is
**practically** empty, or in other words, an empty range.

    Rangeary(RangeExtd::NONE).empty?          # => false
    Rangeary(RangeExtd::NONE).empty_element?  # => true

As mentioned, all the methods of Array but a few are inherited to this
{Rangeary} class, and they in principle work as if `Rangeary` is an Array of
`RangeExtd` (which is indeed the case!).  Four methods work differently:
{Rangeary#+} and {Rangeary#*} are the alias to {Rangeary#disjunction} and
{Rangeary#conjunction}, respectively.  {Rangeary#===} performs +Range#===+ for
all the Rangeary element ranges and return true if any of them returns true. 
Therefore, {Rangeary#===}(RangeExtd(**)) returns always false.  Also,
`#length` and `#reverse` are undefined.  Finally, {Array#==} is modified (see
below).

{Rangeary#==} and +Rangeary#eql?+ work in the same way as of Array. Therefore,
    [2..4, 6..8] == Rangeary(2..4, 6..8)  # => true

However, you should note the following:

    [2..4, 6..8] == Rangeary(6..8, 2..4)  # => true
    [6..8, 2..4] == Rangeary(6..8, 2..4)  # => false

because Rangeary always sort its contents at the initialization. In short, it
is really not recommended to compare Array and Rangeary directly. Instead, you
should compare two {Rangeary} objects.

All the other methods operating on the element ranges, rather than on the
ranges themselves, have a suffix of `_element`, if there is the same method
name in the built-in Array.  For example, +Rangeary#size+ returns the number
of `RangeExtd` objects it holds as an Array, and {Rangeary#size_element}
returns the total +Range#size+ of all the RangeExtd objects it holds.

    Rangeary(1..3, 5..8).size          # => 2
    Rangeary(1..3, 5..8).size_element  # => 7

Or, {Rangeary#flatten_element} returns the concatenated single array of
+Rangeary#to_a+ for all the discrete range elements (the element ranges have
to be discrete like Integer).

To flatten an Array containing both Arrays and Rangeary while you do not want
to flatten each Rangeary, use {Rangeary.flatten_no_rangeary}.

The complete reference of the class and methods is available at [Rubygems
website](http://rubygems.org/gems/rangeary), or you can compile the reference
with `yard` from the source package (+make doc+ at the package root directory
would do).

### Infinities

The infinities are vital in the logical operation of Rangeary. Without it,
negation could not be defined, and other logical operations are also closely
related to it; for example, **subtraction** is basically a combination of
**negation** and **conjunction**.

To determine what the positive and negative infinities for the given elements
is not a trivial task. In default, `nil` is used except for `Numerics`
(Integer, Rational, Float etc), for which `Float::INFINITY` is used. Note that
the default used to be  `RangeExtd::Infinity::POSITIVE` and 
`RangeExtd::Infinity::NEGATIVE` defined in
[RangeExtd](http://rubygems.org/gems/range_extd)  up to Rangeary Ver.1, where
both beginless and endless Ranges were not been introduced or supported.
Rangeary Ver.2 changes the specification to be in line with the latest Ruby
Range.

Alternatively, a user can specify their own infinities in initialization of
{Rangeary} with options of `positive:` and `negative:`. The boundaries at the
opposite polarities usually should match unless they are comparable or either
of them is `nil`.

Here are examples of how infinities work with {Rangeary}. In the first
example, infinities are implicitly contained in the specified Range. Then, the
infinities are internally preserved throughout operations. Note that the first
set of 2 operations and the second set of a single operation means the same.

    r1 = Rangeary(nil..Float::INFINITY).conjunction( RangeExtd::NONE )
      # => Rangeary(RangeExtd::NONE)
    r2 = r1.negation
      # => Rangeary(nil..Float::INFINITY)

    ~(Rangeary(nil..Float::INFINITY) * RangeExtd::NONE)
      # => Rangeary(nil..Float::INFINITY)

In the second example below, a negative infinity of "`d`" is explicitly
specified for a Range of single alphabet String.

    Rangeary("f".."k", negative: "d").negation
      # => Rangeary("d"..."f", "k"<..nil)

where +"k"<..nil+ means a begin-exclude Range or +RangeExtd("k"..nil, true)+.

A note of caution is that once an infinity is defined for a Rangeary object,
any other Rangeary objects with which operations are performed should be in
line with the same infinities.  If you specify your own infinities, it is
advised to do so at the beginning.  And once the infinities have been manually
set, it is advised not to modify them (although this library should handle
such changes appropriately -- see the method document for detail) because
unexpected errors may occur. Here is a set of examples. 

    r3 =  Rangeary("f".."k", negative: "d")
    r4 = ~r3
      # => Rangeary("d"..."f", "k"<..nil)
     _ =   Rangeary(r4, positive: "t")  # raises ArgumentError(!):
                                        # because "t" is smaller than end of Endless Range
    r6 =   Rangeary(r3, positive: "t")  # OK: because end of r3 is only "k"
    r7 =  ~r6
      # => Rangeary("a"..."d", "f".."k") # differs from r4 in the second Range

In the example above, `r4`, which is the negation of `r3` is "Endless", i.e.,
the last Range in `r3` is an endless Range. So, attempting to set a positive
infinity of "`t`" raises an Exception (`ArgumentError`).

If the new infinity(ies) does not contradict the current contents (like `r6`),
it is set accordingly and the subsequent operations (e.g., `r7`) adopt the
value (though you should make sure it is exactly what you want).

#### Algorithm of determining default infinities

Callers can supply user-defined infinity objects for both or either positive
and negative infinity and in that case they are accepted as the infinities
with the highest priority, though ArgumentError might be issued if they
contradict the elements; for example, if a {Rangeary} instance consists of an
array of Integer Ranges (RangeExtd) like +(3..8)+, and yet if String "abc" is
specified as an infinity, it **contradicts** the elements in the sense they
are not comparable.

Internally, the {Rangeary} instance has a Hash extended with
{Rangeary::Util::HashInf}, which can be obtained with {Rangeary#infinities}.
It has only 2 keys of `:negative` and `:positive`, the values of which are the
current best-guessed or definite infinities.  The Hash also holds status
information for each polarity with 3 levels of

1.  `false`
2.  `:guessed`
3.  `:definite`


It is `false` only when the Rangeary is absolutely void with no information
about the contents: +Rangeary(RangeExtd::NONE)+.

If the user explicitly specifies a boundary in the optional arguments in
initialization of {Rangeary}, it is accepted in principle with an associated
status of `:definite`.

If the user-specified main arguments in initialization contain a (potentially
multiple) {Rangeary}, their defined infinities are inherited with their
associated statuses. 

Also, user-supplied Range-s or RangeExtd-s to the arguments in initialization
of {Rangeary} always have, except for `RangeExtd::NONE`, concrete boundary
values, which can be `nil`.

If one of the boundaries of a Range (n.b., it is **not** Rangeary) contains
either `nil` or one of infinite values (which is checked with
`RangeExtd::Infinity.infinite?`, where in practice a duck-typing check is
performed, using the method `infinite?`), then it is accepted as an infinite
value with an associated status of `:definite`.

Otherwise,

1.  if a boundary value is a (real-type) Numeric, `Float::INFINITY` (or its
    negative,
2.  or otherwise, `nil`


is set as an infinity of the boundary with an associated status of `:guessed`.

Note that the priority used to be different up to Rangeary Ver.1; `nil` was
not used and instead the `RangeExtd::Infinity` objects were used. It was
because the beginless Range (and endless Range before Ruby-2.6) has not been
defined before Ruby 2.7.  Now they are defined, it is only natural to use
`nil` as the default infinities in both ends, hence the change in
specification in {Rangeary} Ver.2.

Usually the arguments given in initialization of a {Rangeary} contain more
than one set of infinities candidate, unless only a single argument of either
Range (or its subclass instance) with no optional arguments is given.  The
priority is judged in the following order:

1.  the optional arguments
2.  an associated status of `:definite`, `:guessed`, and `false` in this order


If the associated statuses are equal for two or more inputs, the most extreme
one among them for each polarity is chosen.  For example, suppose two
instances of {Rangeary} are given in initialization of another {Rangeary} and
their negative infinities are "`b`" and "`c`".  Then, because of

    "b" < "c"

the former ("`b`") is adopted as the new negative infinity.  Note that the
parameters given in the optional arguments have always higher priority
regardless.

The following examples demonstrate the specification.

    Rangeary(7..).negation
      # => Rangeary(-Float::INFINITY...7)
    Rangeary(7..).negation.negation
      # => Rangeary(7..)

Remember the default infinity for Float is `Float::INFINITY`. In this case,
however, the positive infinity was in practice specified by the user to be
`nil` in the form of argument of +(7..)+ If you want to specify the negative
infinity instead, you must do it explicitly: 

    Rangeary(7.., negative: nil).negation
      # => Rangeary(...7)

Alternatively, you can always use conjunction like (the following two mean the
same):

    Rangeary(..nil).conjunction(Rangeary(7..)).negation
      # => Rangeary(...7)
    ~(Rangeary(..nil) * Rangeary(7..))
      # => Rangeary(...7)

The registered infinities for each instance is obtained (Hash extended with
HashInf), which has two keys of `:positive` and `negative`, with the method
{#infinities}; for example,

    ran.infinities
      # => <Hash(Inf): {:negative=>"a", :positive=>nil},
      #     status: {:negative=>:definite, :positive=>:guessed}>

Note that the values of the returned Hash (+HashInf) may be `false`; if it is
not convenient, call it as +#instances(convert: true)+ with which `false` in
the returned value, if there is any, is converted to `nil` and the standard
Hash as opposed to Hash extended with {Rangeary::Util::HashInf} is returned:

    ran.infinities(convert: true)  # => { :negative => "a"
                                   #      :positive => nil, }

Consult the manuals of the methods for detail.

### Array#==

Equal method of +Array#==+ (and thus its child-class +Rangeary#==+) is
modified slightly so that the behaviour when both are **practically** empty.

Rangeary objects are never empty in the sense of Array; it contains at least
`RangeExtd::NONE`, which is **empty**.  Therefore, if either or both the
Array/Rangeary return true with {Rangeary#empty_element?}, then it is regarded
as equivalent to +Array#empty?+.

Up to Ver.1 (or more precisely, in Ver.1), the equality behaviour was far more
complicated. The complexity originated in the incompleteness of the Ruby
built-in borderless Range; while Ruby-2.6 introduced the endless Range, which
Rangeary Ver.1 supported, it lacked the beginless Range till the release of
Ruby-2.7.

Since Rangeary Ver.1 implemented only the endless Range, logical operations,
especially those that involve negation either explicitly or implicitly (like
subtraction), were self-incomplete.  To mitigate the problem, the equality
method was designed to handle the incompleteness instead in Rangeary Ver.1.0.

Now that both beginless and endless Ranges are supported by Ruby and Rangeary
Ver.2, such an ad hoc fix is no longer necessary or desirable. The difference
from the Ruby default is now minimum.

Note that this library loads the utility library associated with `RangeExtd`,
which modifies some behaviours of the equality method (operator) of all
Object, if in a backward-compatible way, i.e., users do not have to worry
about it for the use outside RangeExtd. For logical operations implemented in
this library, commutative behaviours of the operators are essential and they
would be only achieved by modifying +Object#==+.

Also note that +Rangeary#equiv+ method may behave differently from the equal
operator. For example, 

    Rangeary(RangeExtd(1,"<...",4), 5...8).equiv?(Rangeary(2..3, 5..7))

returns true.  To describe this, the left and right Rangearies are arrays of
ranges of Integers which consist of

*   (Left)
    1.  Range that starts at, but excluding, 1, ending at, but excluding 4
    2.  Range that starts at 5 (inclusive), ending at 8 (exclusive).

*   (Right)
    1.  Range that starts and ending at 2 and 3, respectively, both inclusive.
    2.  Range that starts and ending at 5 and 7, respectively, both inclusive.



According to +Integer#succ+ (+Range#each+) method, the left and right ones are
equivalent. Therefore +Rangeary#equiv+ returns true, whereas the equal
operator returns false for this.

## Known bugs

*   Rangeary Ver.2, which supports both beginless and endless Ranges (of Ruby
    2.7 and later), requires [RangeExtd](https://rubygems.org/gems/range_extd)
    Ver.2 or later.
*   To suppress warnings in Ruby-2.7 (in Rangeary Ver.1),
    [RangeExtd](https://rubygems.org/gems/range_extd) must be Ver.1.1.1 or
    later.
*   +Rangeary#last_element+ includes a monkey patch (which used to raise an
    error before Rangeary Ver.2) to handle a bug in +Range#last+ in Ruby-2.7
    and above (at least up to 3.1.2).  See [Bug
    #18994](https://bugs.ruby-lang.org/issues/18994) for detail of the bug,
    which was in no time resolved with ([patch
    #6324](https://github.com/ruby/ruby/pull/6324), applied at commit
    [bbe5ec7](https://github.com/ruby/ruby/commit/bbe5ec78463f8d6ef2e1a3571f17
    357a3d9ec8e4)). It happens only in very limited conditions.


This library (for Ver.2 and later) requires Ruby 2.7 or later.

Extensive tests have been performed, as included in the package.

## ToDo

Nothing planned.

## Final comment

This work is inspired by Ian Stewart, who developed a (numeric) multiple-range
library in FORTRAN90 for the SAS software package for XMM-Newton telescope.  I
appreciate his work.

The implementation to Ruby was not straightforward, though, partly because the
built-in Range does not allow to exclude the begin boundary, and partly
because the infinity is not defined for general objects but Numeric (Real) in
Ruby (which later changed partially with the introduction of Endless Range in
Ruby 2.6 released in 2018 December and further and completely with the
introduction of Beginless Range in Ruby 2.7 released in 2019 December). 
`RangeExtd` class I have developed makes this library possible.  I am glad the
completeness of ranges in the 1-dimensional space for arbitrary Comparable
object is achieved now.

Enjoy.


## Miscellaneous

## Copyright etc

Author
:   Masa Sakano < info a_t wisebabel dot com >
License
:   MIT.
Warranty
:   No warranty whatsoever.
Versions
:   The versions of this package follow Semantic Versioning (2.0.0)
    http://semver.org/


---

# Rangeary - 複数Range(Extd)クラス

このパッケージは、1次元の任意の数のレンジ([RangeExtd](http://rubygems.org/gems/range_extd)
オブジェクト)を 保持する Rangeary クラスを定義しています。たとえば、`x` に 対する複数レンジの
    0.5 < x <= 2.5,  6.0 <= x < 8.0,  10.0 <= x (<= 無限大)

がこのクラスで定義できます。

RangeExtd の要素たり得るオブジェクトは全て [Rangeary} の要素となり 得ます。Numeric(の実数)に限らず、Comparable
であるオブジェクトは全て 可能です。また、{Rangeary} は、組込Rangeクラスのオブジェクトも、それが
{RangeExtd](https://rubygems.org/gems/range_extd) クラスの (`Range#valid? ==
true`) を満たす限り初期化に使えます (注: ほとんどの Rangeオブジェクトはこの条件を満たすものの、 +(true..true)+
のように満たさないものもあって、実際、それらは複数 Rangeの構成要素としては意味をなさないものです)。

四つの標準論理演算全て、すなわち否定(negation; `~`)、論理積 (conjunction; `&` または
`*`)、論理和(disjunction; `|` または `+`)および排他的論理和(exclusive disjunction; `^` または
`xor`)、またそれに加えて引き算(subtraction; `-`)が、定義されています。

{Rangeary} オブジェクトはイミュータブルです。すなわち、 一度インスタンスが生成されると、要素を書換えることはできません。

実用的には、{Rangeary} は、Arrayのサブクラスとなっていて、RangeExtd
の配列として振舞います。また、Arrayのほとんどのメソッドを継承していま す。したがって、イミュータブルな{Rangeary}に許される限りにおいて、
Arrayに使えるほとんどの操作を{Rangeary}に適用できます。加えて、
{Rangeary}には、そのレンジの要素に直接働きかけるメソッドも幾つかあり ます。上に挙げた例ならば、`Range#cover?(1.0)`
は真を返し、 `Range#cover?(9.0)` は偽を返します。

このクラスにより、1次元レンジへの論理演算が可能かつ容易になりました。 これが有用なものであることを願ってここにリリースします。

### News: Libraryの場所、Beginless Rangeのサポート他

**重要**: ライブラリのパスが{Rangeary} Ver.1 から Ver.2 で、 ディレクトリの階層一つ上がりました。これは、Ruby
Gemの慣用にそうように するためです。端的には、標準的方法は、+require "rangeary"+ です。
以前のパスは、"rangeary/rangeary" でした。

それに伴い、{Rangeary} のバージョンを2.0にあげました。

Ruby 2.7以上では、 [Beginless
range](https://rubyreferences.github.io/rubychanges/2.7.html#beginless-range)
がサポートされます。

`Rangeary` (Ver.2) もそれをサポートします。ただし、
[RangeExtd](https://rubygems.org/gems/range_extd) Ver.2 (以上) が必要です。

#### News: Endless Range サポートしました

2019年10月(Rangeary Ver.1)より、Ruby-2.6 で導入された [Endless
Range](https://rubyreferences.github.io/rubychanges/2.6.html#endless-range-1)
をついにサポートしました!

## インストール

まず、 [RangeExtd](https://rubygems.org/gems/range_extd) クラス(Ver.2
以上)のライブラリが必要です。gem を使ってインストールできます。
    gem install range_extd

もしくは以下から入手して下さい。 {https://rubygems.org/gems/range_extd}

(同ライブラリを手動でインストールする場合は、同ライブラリのマニュアルを参照。)

次いで、このライブラリをインストールします。
    gem install rangeary

ファイル
    rangeary.rb
    rangeary/util.rb
    rangeary/util/hash_inf.rb
    rangeary/rangeary.rb

が`$LOAD_PATH`上のどこかにインストールされるはずです。  あるいは以下から入手可能です。
{http://rubygems.org/gems/rangeary}

後は、Ruby のコード(又は irb)から
    require 'rangeary'

とするだけです。 (このパスは、Rangeary Ver.1 では "rangeary/rangeary" でした!)

他のファイル(特に`range_extd.rb`)は、自動的に読込まれます。

{Rangeary} Ver.2 は Ruby 2.7 以上で動きます。

お楽しみあれ!

## 単純な使用例

### Rangeary インスタンスを作成する方法

以下に幾つかの基本的な使用例を列挙します。

    r1 = RangeExtd(?a...?d, true) # => a<...d
    ra = Rangeary(?g..?h, r1)     # => [a<...d, g..h]
    Rangeary(RangeExtd::NONE)     # => [RangeExtd::NONE]
    Rangeary(RangeExtd::NONE, 1..5, 3...7)  # => [1...7]
    Rangeary(true..true)          # => ArgumentError

基本的に、`Rangeary()` または `Rangeary.new()` は、任意 の数の Range、RangeExtd、{Rangeary}
あるいはその組合わせを引数とし て取ります。ただし、Rangeクラスのオブジェクトで +Range#valid?+ が偽 を返すものは、例外が発生します。

さらなる解説及び例は、{Rangeary.initialize}を参照して下さい。

### 実践例

    ra.to_a                       # => ["a"<..."d", "g".."h"]
    ra.cover?("a")                # => false
    ra.cover?("b")                # => true
    ra.end                        # => "h"
    ra.each do |i|
      print i.begin
    end    # => self ( "ag" => STDOUT )
    ra.each_element do |i|
      print i
    end    # => self ( "bcgh" => STDOUT )

    rb = Rangeary(6...9, 2..4)    # => [2..4, 6...9]
    rb + Rangeary(3..7)           # => [2...9]
    rb - Rangeary(3..7)           # => [2...3, RangeExtd(7,'<...',9)]
    rb * Rangeary(4..5, 8..10)    # => [4..4, 8...9]
    ~rb                           # => [-Float::INFINITY...2, RangeExtd(4,'<...',6), 9..Float::INFINITY]

    rc = Rangeary(?d...?x, negative: ?a, positive: ?z)
    ~rc                           # => [?a...?d, ?x..?z]

ここで、+~rb+ は `rb.negation` と等価で論理否定を意味します。最後の例 では、ユーザー指定の*無限大/無限小*値を定義しています。

組込Arrayクラスに含まれるほとんどのメソッドが、たとえば+Array#push+のように {Rangeary}
がイミュータブルであることに抵触しない限りにおいて、使用可能です。

## 詳説

ファイル `rangeary.rb` が読まれた段階で、 [RangeExtd](https://rubygems.org/gems/range_extd)
ライブラリで定義されるクラス (`RangeExtd`, `RangeExtd::Infinity`, `RangeExtd::Nowhere`)
が読み込まれます。 `RangeExtd` は、組込み built-in `Range` クラスにいくつかのメソッドを追加します。

### Rangeary クラス

{Rangeary} オブジェクトは、Range や RangeExtd と同様、イミュータブルで
す。だから、一度インスタンスが生成されると、変化しません。

インスタンスの生成方法は上述の通りです(「使用例」の章)。レンジとして"valid" と見なされないインスタンスを生成しようとすると、すなわち
+Range#valid?+ が 偽を返すレンジを使おうとすると、例外(`ArgumentError`)が発生し、 失敗します。

`Rangeary`初期化引数の `RangeExtd` (または `Range`) オブジェクトは
(どちらかあるいは両方の境界に)無限大オブジェクトを含むかも知れません。 Ruby 2.7 と2.6にて、 [Beginless
range](https://rubyreferences.github.io/rubychanges/2.7.html#beginless-range),
[Endless
Range](https://rubyreferences.github.io/rubychanges/2.6.html#endless-range-1)
がそれぞれ導入されました。これらは、本ライブラリに当初から定義されていた
[RangeExtd::Infinity](https://www.rubydoc.info/gems/range_extd/RangeExtd/Infin
ity) に対応すると言えます。

組込みのこれら境界のない Ranges と `RangeExtd::Infinity` オブジェクトは
類似しているものの概念としては少し異なります。この違いは、 境界のない Ranges と`Float::INFINITY` との違いに似ています。
[RangeExtd](http://rubygems.org/gems/range_extd) のマニュアルに詳細に解説されています。

デフォルトでは、引数に与えられたRangeがこれら無限大オブジェクト
(`nil`であれ`Float::INFINITY`であれ`RangeExtd::Infinity`であれ) を含む場合、`Rangeary`
はそれをそのまま使います。あるいは、 ユーザーは、{Rangeary} インスタンス生成の時、
レンジ要素に対応した正負の無限大オブジェクトを指定することもできます。 たとえば、1文字のStringのRangeに対して、+"a"+
を負の無限大オブジェクト として指定することができます。もし何も指定がない場合は、Rangearyは、 デフォルトで、 Numeric (実数)
に対しては`Float::INFINITY`、 それ以外には `nil` を無限大とします。

生成時に複数のレンジが引数として与えられた時、ソートされて保持されます。 その時、要素のどこかに重複する部分があった時は、論理和として扱われます
(つまり、単純に足し合わされます)。これはつまり、{Rangeary} が内部的に 保持するオブジェクトは生成時に与えられたものとは異なる、すなわち
`#object_id` が異なるかも知れないことを意味します。特に、組込Rangeが引 数として与えられた時は、常に RangeExtd
オブジェクトに内部で変換されます。

もし生成時に与えられたレンジのどれかが空、すなわち +Range#empty?+ が真
を返す場合、それらは無視されます。ただし、引数の全てのレンジが空であっ た場合は、「最小」のものが残されます。

もし演算の結果として空の{Rangeary}が返される場合、その唯一の要素は `RangeExtd::NONE`となり、したがって
{Rangeary#empty_element?} が真を返します。

そのため、どの Rangeary オブジェクトも、+Rangeary#to_a.size+ は常に正
の値を返し、零を返すことはありません。あるいは、Array から継承した +Rangeary#empty?+
は常に偽を返します(オブジェクトがレンジとして空かど うかをチェックするには、{Rangeary#empty_element?} を使って下さい)。
    Rangeary(RangeExtd::NONE).empty?          # => false
    Rangeary(RangeExtd::NONE).empty_element?  # => true

前述のように、ごく一部を除いた全ての Arrayクラスのメソッドがこの {Rangeary}
クラスに継承されていて、それらは各RangeExtd要素に対して 動作します。ただし、4個のメソッドの挙動が異なります。 {Rangeary#+} と
{Rangeary#*} とは、それぞれ {Rangeary#disjunction} と {Rangeary#conjunction}
とへのエイリアスとなっています。{Rangeary#===} は、全てのRangeExtd要素に対して+Range#===+を実行し、それらの一つでも
真を返せば真を返します。したがって、{Rangeary#===}(RangeExtd(**))は常 に偽を返します。 [また、](#length) と
[#reverse] とは未定義化されています。

{Rangeary#==} と +Rangeary#eql?+ は、Arrayと同様に動作します。だから
    [2..4, 6..8] == Rangeary(2..4, 6..8)  # => true

も成り立ちます。しかし注意すべきは以下です。
    [2..4, 6..8] == Rangeary(6..8, 2..4)  # => true
    [6..8, 2..4] == Rangeary(6..8, 2..4)  # => false

端的には、標準Arrayとの直接比較は推奨しません。代わりに、 {Rangeary} オブジェクトと比較して下さい。

レンジ要素に対してではなく、レンジを構成する要素に対して動作する他の 全てのメソッドは、組込Arrayクラスに同名のメソッドが存在する場合、接尾辞
`_element` がつきます。たとえば、+Rangeary#size+ は、保持する RangeExtd
オブジェクトの数を返し、一方、{Rangeary#size_element}は、 保持するすべての RangeExtdオブジェクトに対して
+Range#size+ を行った その総和を返します。
    Rangeary(1..3, 5..8).size          # => 2
    Rangeary(1..3, 5..8).size_element  # => 7

{Rangeary#flatten_element} は、全てのRangeExtd要素に対して +Rangeary#to_a+
を実行して、その結果を結合した配列を返します。

もしArrayをflattenしたいけれど各Rangeary は保持したい場合は、 {Rangeary.flatten_no_rangeary}
を使えます。

クラスとメソッドの完全なマニュアルは、 [Rubygems
のウェブサイト](http://rubygems.org/gems/rangeary)上にあります。
あるいは、ソースパッケージを展開して、ルートディレクトリで +make doc+ することで、手元でコンパイルすることもできます(RubyGems の
`yard` がインストールされている必要があります)。

### 無限大

無限大の扱いは単純ではありません。

最大の注意点として、一旦、無限大が定義されると、そのオブジェクトと演算を行う全ての Rangearyオブジェクトも同じ無限大をもつべきです。

取扱いと挙動の詳細は英語版マニュアルの「Infinities」章を参照してください。

### Array#==

等号メソッド +#==+ は、Array とはほんの少し異なった挙動をします。 というのも、**empty?** の意味が、異なるからです。なぜならば、
Rangeary オブジェクトは、Array的な意味では、empty になることは決してありません。
したがって、{Rangeary#empty_element?} または通常の **empty?** が双方真の時は+Rangeary#==+
は真を返すように +Array#==+ (したがって +Rangeary#==+)が変更されています。

Rangeary Ver.1.0 では、 Ruby 2.6で導入された [Endless
Range](https://rubyreferences.github.io/rubychanges/2.6.html#endless-range-1)
を概念的に素直に組み込むのが困難であり、Rangeary 内部で矛盾ない扱いを保証するために、 +Range#==+ に複雑な変更を含みました。Ruby
2.7 で Beginless Range が導入されたことでその矛盾が自然に解消されたため、Rangeary Ver.2.0
では、その複雑な判断ルーチンは撤廃されました。

本ライブラリは `RangeExtd` 付属ライブラリを読み込み、そこでは Object
の等号が少し変更されています(Ruby標準と互換性はあるので心配無用)。 これは、Rangeary内部で演算の双方向性を保証するために必要な措置です。

なお、+Rangeary#equiv+ メソッドは、等号とは異なる挙動をすることがあり ます。例えば、

    Rangeary(RangeExtd(1,"<...",4), 5...8).equiv?(Rangeary(2..3, 5..7))

は真になります。以下のような説明になります。左右の Rangearyは、整数の Rangeからなる配列で、

*   (左側の Rangeary)
    1.  始端が1 (但し始端自体は含まない)で、終端が4 (但し終端自体は含まない)
    2.  始端が5 (始端を含む)で、終端が8 (但し終端自体は含まない)

*   (右側の Rangeary)
    1.  始端が2、終端が3 (両端を含む)
    2.  始端が5、終端が7 (両端を含む)



+Integer#succ+ (あるいは、+Range#each+)メソッド的には、左側と右側とは等価です。したがって、 +Rangeary#equiv+
でこの両者を比較すると真を返します。一方、等号は負を 返します。

## 既知のバグ

*   Rangeary Ver.2 は Beginless/Endless Ranges (Ruby 2.7以降)をサポートしますが、そのために、
    [RangeExtd](https://rubygems.org/gems/range_extd) Ver.2 以降が必須。
*   Ruby-2.7 で警告を出さないためには、[RangeExtd](https://rubygems.org/gems/range_extd)
    は、Ver.1.1.1 以上であること。
*   +Rangeary#last_element+ はちょっと汚いパッチが入っています(Rangeary Ver.1
    では例外が発生していた)が、これは、Ruby-2.7以降(3.1.2でもまだ未修正) のバグに対処するためです。バグ詳細は、 [Bug
    #18994](https://bugs.ruby-lang.org/issues/18994)。 すぐに対処されました([patch
    #6324](https://github.com/ruby/ruby/pull/6324), コミット
    [bbe5ec7](https://github.com/ruby/ruby/commit/bbe5ec78463f8d6ef2e1a3571f17
    357a3d9ec8e4))。 そもそもごくごく限られたケースでしか問題になることはありませんが。
*   `Rangeary.new(-6..-5, 2..5, 8..8).last_element(3)` は、
        Ruby-2.6.x 以前は正しい値を返すが、Ruby-2.7 ではなぜか <tt>[3, 4, 5]</tt> と誤った値を返す。


このライブラリは Ruby 2.7 以上を必要とします。

パッケージに含まれている通り、網羅的なテストが実行されています。

## 未処理事項

特になし。

## 終わりに

このライブラリは、イアン・スチュワート氏が開発した、XMM-Newton望遠鏡用の SAS解析ソフトウェア・パッケージに含まれる(数値)複数レンジの
FORTRAN90ライブラリにアイデアを得たものです。彼の仕事に感謝します。

しかし、Rubyへの実装は、一筋縄ではいきませんでした。一つには組込Range クラスでは始点を除外することができないこと、また一つには Ruby
では数値を除いて一般オブジェクトに対しての無限大が定義されていないからです
(後者は、Ruby-2.6と2.7とで一定の解決を見たため、本ライブラリ(Ver.2)でも採り入れました!)。 小生の開発した RangeExtd
によって初めてこのライブラリが可能となりました。 これにより、今、比較可能な任意のオブジェクトについて、
1次元上のレンジの完全性が実現できたことを嬉しく思います。

お楽しみ下さい。

## その他

## 著作権他情報

著者
:   Masa Sakano < info a_t wisebabel dot com >
利用許諾条項
:   MIT.
保証
:   一切無し。
バージョン
:   Semantic Versioning (2.0.0) http://semver.org/


