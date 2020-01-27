
# Rangeary - Multiple Range(Extd) class

This package defines Rangeary class, which contains any multiple 1-dimensional
ranges ([RangeExtd](http://rubygems.org/gems/range_extd) objects).  For
example, a multiple range of

    0.5 < x <= 2.5,  6.0 <= x < 8.0,  10.0 <= x (<= Infinity)

for `x` can be defined in this class.

The element objects for {Rangeary} can be anthing that can form RangeExtd
objects; not only Numeric (or Real) but anything Comparable.  {Rangeary}
accepts the built-in Range-class objects for the elements in initialisation,
too.

All the four standard logical operations, that is, negation (`~`), conjunction
(`&` or `*`), disjunction (`|` or `+`) and exclusive disjunction (`^` or
`xor`) are defined, as well as subtraction (`-`).

{Rangeary} objects are immutable - once it is created, you can not alter the
contents.

Practically, {Rangeary} is a sub-class of Array, works as an array of
RangeExtd, and inherits most of the methods, hence you can apply most
operations to {Rangeary} that Array accpets, within the restriction of
immutability of {Rangeary}. In addition, {Rangeary} offers several methods
that directly work on its element as a range.  In the above example,
`#cover?(1.0)` would return true, whereas `#cover?(9.0)`, false.

With this class, logical operations of 1-dimensional range objects are now
possible and easy. I hope you find it to be useful.

### News: Endless Range supported

Now, as of 2019 October, this fully supports [Endless
Range](https://rubyreferences.github.io/rubychanges/2.6.html#endless-range-1)
introduced in Ruby 2.6.  It is released as Version 1.* finally!

## Install

First, you need [RangeExtd](https://rubygems.org/gems/range_extd) class
library (basically, two files of pure ruby code), which is in gem:

    gem install range_extd

Or, get it from https://rubygems.org/gems/range_extd if you have not yet
installed it.

Then, install this library.

    gem install rangeary

A file

    rangeary/rangeary.rb

should be installed in one of your `$LOAD_PATH`. Alternatively, get it from
http://rubygems.org/gems/rangeary

Then all you need to do is

    require 'rangeary/rangeary'

or, possibly as follows, if you manually install it

    require 'rangeary'

in your Ruby script (or irb).  The library files like
`range_extd/range_extd.rb` for RangeExtd are automatically loaded.

{Rangeary} itself may work in Ruby 1.8, but RangeExtd works in only Ruby 2.1
or later.

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
objects that return false in +Range#valid?+ will raise an exception.

For more detail and examples, see {Rangeary.new}.

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
addtion to the two defined in the RangeExtd library (`RangeExtd` and
`RangeExtd::Infinity`).

### Rangeary Class

{Rangeary} objects are immutable, the same as Range and RangeExtd. Hence once
an instance is created, it would not change.

How to create an instance is explained above (in the Examples sections).  Any
attempt to try to create an instance with one of elements being not "valid" as
a range, that is, +Range#valid?+ returns false, raises an exception
(`ArgumentError`), and fails.

Note `RangeExtd` (or `Range`) objects used to initialise [Rangeary} instances
can contain negative and/or positive infinity objects. Since Ruby 2.6 the
latter is called {Endless
Range](https://rubyreferences.github.io/rubychanges/2.6.html#endless-range-1)
and is supported as built-in Range.  The former is defined only in
[RangeExtd](https://rubygems.org/gems/range_extd) class. In default they are
of the type of either `Float::INFINITY` or either of the two instances of
[RangeExtd::Infinity](https://www.rubydoc.info/gems/range_extd/RangeExtd/Infin
ity) class, `POSITIVE` or `NEGATIVE`.  See {Rangeary.new} for detail about how
to initialise.

When multiple ranges are given in initialising, they are sorted in storing,
and if there are any overlaps among any of the elements, they are treated as
disjunction (that is, simple summation).  That means the objects a {Rangeary}
instance holds internally can be different from  the objects given in
initialisation, namely, their {#object_id} may be different.  In particular,
if built-in Range objects are given, they are always converted into RangeExtd
objects internally.

If any of the given range in the intialisation is empty, that is,
+Range#empty?+ returns true, they are ignored, unless all of the ranges given
are empty ranges, in which case the "smallest" one will be preserved.

If the result of the operation is empty, only the element of the resultant
{Rangeary} is `RangeExtd::NONE`, and hence {Rangeary#empty_element?} will
return true.

For any Rangeary objects, {Rangeary#to_a}.size is always positive and not zero
for that reason, or {Rangeary#empty?} returns always false, as
{Rangeary#empty?} is a method inherited from Array. Use
{Rangeary#empty_element?} instead to check whether the instance is
**practically** empty or not as a range.

    Rangeary(RangeExtd::NONE).empty?          # => false
    Rangeary(RangeExtd::NONE).empty_element?  # => true

As mentioned, all the methods of Array but a few are inherited to this
{Rangeary} class, and they work based on each element RangeExtd.  Four methods
work differently: {Rangeary#+} and {Rangeary#*} are the alias to
{Rangeary#disjunction} and {Rangeary#conjunction}, repectively. 
{Rangeary#===} performs +Range#===+ for all the Rangeary element ranges and
return true if any of them returns true.  Therefore,
{Rangeary#===}(RangeExtd(**)) returns always false.  Also, `#length` and
`#reverse` are undefined.  Finally, {Array#==} is modified (see below).

All the other methods operating on the element of ranges, rather than on the
ranges themselves, have a suffix of `_element`, if there is the same method
name in the built-in Array.  For example, {Rangeary#size} returns the number
of RangeExtd objects it holds, and {Rangeary#size_element} returns the total
+Range#size+ of all the RangeExtd objects it holds.

    Rangeary(1..3, 5..8).size          # => 2
    Rangeary(1..3, 5..8).size_element  # => 7

Or, {Rangeary#flatten_element} returns the concatnated single array of
{Rangeary#to_a} for all the discrete range elements (the element ranges have
to be discrete like Integer).

The complete reference of the class and methods is available at [Rubygems
website](http://rubygems.org/gems/rangeary), or you can compile the reference
with `yard` from the source package (+make doc+ at the package root directory
would do).

### Array#==

Equal method of +Rangeary#==+ should work differently from Array. First, it
behaves differently for **empty** objects, as Rangeary objects are never empty
in the sense of Array.  Second, handling of  [Endless
Range](https://rubyreferences.github.io/rubychanges/2.6.html#endless-range-1)
introduced in Ruby 2.6 is conceptionally not straightforward. For example, in
Ruby 2.6,

    (?a..).size                  # => nil
    (10..).size                  # => Infinity
    (10..Float::INFINITY)).size  # => Infinity
    (1.0..) != (1.0..Float::INFINITY)

These are not trivial, or even a little inconsistent.  For Range#size method,
if it is for Numeric (but Complex), Endless Range behaves like the number
Infinity, yet they (the mathematical infinity and the end of Endless Range) do
not compare, and are not equal.

Rangeary offers functions of logical operations of arrays of Ranges. A
consistent definitions of infinity is essential.  For example, the negation of
+[5..Infinity]+ is

    [-Infinity...5]

that is, from the negative infinity to 5, excluding the end.  Then the
negation of it must recover the original Rangeary +(5..Infinity)+.

Obviously, it could not be done with +Endless Range+, as it defines only the
positive infinity, or more accurately *positive endlessness*.  For that
reason, the use of `RangeExtd` is crucial for Rangeary.

In the Rangeary operation (of negation in this case), the following can
happen:

    r1 =   Rangeary(5..)   # => [(5..)]  (equivalent to (5..nil))
    r2 =  ~Rangeary(5..)   # => [(-Float::INFINITY...5)]
    r3 = ~~Rangeary(5..)   # => [(5..Float::INFINITY)]

    r4 =   Rangeary(?a..)  # => [(?a..)]  (equivalent to (?a..nil))
    r5 =  ~Rangeary(?a..)  # => [(RangeExtd::Infinity::NEGATIVE...?a)]
    r6 = ~~Rangeary(?a..)  # => [(?a..RangeExtd::Infinity::POSITIVE)]

There is no reason for this operation to result in the (Ruby-2.6) Endless
Range that began with, after the negation operation from the negative
infinity. Nevertheless, +r1==r3+ and +r4==r6+ must hold.

+RangeExtd#==+ behaves in an almost identical way to +Range#==+ (except it has
`exclude_begin`), such as,

             (1.0..) !=          (1.0..Float::INFINITY)
    RangeExtd(1.0..) != RangeExtd(1.0..Float::INFINITY)

To maintain the mathematical consistency, Rangeary needs modify the equal
operator so +r1==r3+ and +r4==r6+ hold, as the standard equal operator of
Array would not do the job. Hence, Rangeary now regards an Endless Range and a
Range that ends with another Infinity as equal, as long as all the other Range
parameters like the "begin" and `exclude_end` are equal.

In practice, +Array#==+ is modified in this library (which is naturally
inherited to Rangeary), and hence the commutativity of the equal operator is
assured.

Note that +Rangeary#equiv+ method may behave differently from the equal
operator. For example, 

    Rangeary(RangeExtd(1,"<...",4), 5...8).equiv?(Rangeary(2..3, 5..7))

returns true.  To describe this, this left and right Rangearies are arrays of
ranges of Integers which consist of

*   (Left)
    1.  Range that starts at, but excluding, 1, ending at, but excluding 4
    2.  Range that starts at 5 (inclusive), ending at 8 (exclusive).

*   (Right)
    1.  Range that starts and ending at 2 and 3, respectively, both inclusive.
    2.  Range that starts and ending at 5 and 7, respectively, both inclusive.



According to +Integer#succ+ (+Range#each+) method, the left and right ones are
equivalent. Therefore +Rangeary#equiv+ returns true, wheras the equal operator
returns false for this.

### Infinities

The infinities are vital in the logical operation of Rangeary.  In default,
the general infinities of `RangeExtd::Infinity::POSITIVE` and 
`RangeExtd::Infinity::NEGATIVE` are used (see
([RangeExtd](http://rubygems.org/gems/range_extd) for detail), except for
comparable Numerics (Integer, Rational, Float etc), for which +Float::INFINITY
is used in default.  However, a user can specify their own infinities in
initialisation with the options of `positive:` and `negative:`.

Note once an infinity is defined for a Rangeary object, any other Rangeary
objects with which operations are performed should have the same infinities. 
In default, if different infinities are specified (or if only one of them has
a pair of specified infinities and the others have the default values), the
values that is smallest for the Positive infinity and largest for the Negative
are used, though a warning may be issued (only if the built-in global variable
+$VERBOSE+ is true).  An exception is when a user explicitly specifies the
infinities in the option in creating a new Rangeary out of other Rangeary
objects and else, in which case the infinities from the other "inherited"
Rangeary included in the main parameter are ignored.

The last example above shows how it works.

## Known bugs

*   To suppress warnings in Ruby-2.7,
    [RangeExtd](https://rubygems.org/gems/range_extd) must be Ver.1.1.1 or
    later.
*   `Rangeary.new(-6..-5, 2..5, 8..8).last_element(3)` returns
        <tt>[3, 4, 5]</tt> wrongly in Ruby-2.7 but correctly in Ruby-2.6.x or earlier.


This library requires Ruby 2.1 or above (it may work all right with Ruby
1.9.3, however I have never tested it).

Extensive tests have been performed, as included in the package.

## ToDo

Nothing planned.

## Final comment

This work is inspired by Ian Stewart, who developped a (numeric)
multiple-range library in Fortran90 for the SAS software package for
XMM-Newton telescope.  I appreciate his work.

The implementation to Ruby was not straightforward, though, partly because the
built-in Range does not allow to exclude the begin boundary, and partly
because the infinity is not defined for general objects but Numeric (Real) in
Ruby (which later changed partially with the introduction of Endless Range in
Ruby 2.6 released in 2018 December).  RangeExtd class I have developed makes
this library possible.  I am glad the completeness of ranges in the
1-dimensional space for arbitrary Comparable object is achieved now.

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

RangeExtd の要素たり得るオブジェクトは全て {Rangeary} の要素となり 得ます。Numeric(の実数)に限らず、Comparable
であるオブジェクトは全て 可能です。また、{Rangeary} は、組込Rangeクラスのオブジェクトも初期化に 使えます。

四つの標準論理演算全て、すなわち否定(negation; `~`)、論理積 (conjunction; `&` または
`*`)、論理和(disjunction; `|` または `+`)および排他的論理和(exclusive disjunction; `^` または
`xor`)、またそれに加えて引き算(subtraction; `-`)が、定義されています。

{Rangeary} オブジェクトはイミュータブルです。すなわち、 一度インスタンスが生成されると、要素を書換えることはできません。

実用的には、{Rangeary} は、Arrayのサブクラスとなっていて、RangeExtd
の配列として振舞います。また、Arrayのほとんどのメソッドを継承していま す。したがって、イミュータブルな{Rangeary}に許される限りにおいて、
Arrayに使えるほとんどの操作を{Rangeary}に適用できます。加えて、
{Rangeary}には、そのレンジの要素に直接働きかけるメソッドも幾つかあり ます。上に挙げた例ならば、`#cover?(1.0)` は真を返し、
`#cover?(9.0)` は偽を返します。

このクラスにより、1次元レンジへの論理演算が可能かつ容易になりました。 これが有用なものであることを願ってここにリリースします。

## インストール

まず、RangeExtd クラスのライブラリ(純粋に rubyで書かれたファイル 2個 です)が必要です。gem を使ってインストールできます。
    gem install range_extd

もしくは以下から入手して下さい。 https://rubygems.org/gems/range_extd

次いで、このライブラリをインストールします。
    gem install rangeary

ファイルが一つ、
    rangeary/rangeary.rb

`$LOAD_PATH`上のどこかにインストールされるはずです。  あるいは以下から入手可能です。
http://rubygems.org/gems/rangeary

後は、Ruby のコード(又は irb)から
    require 'rangeary/rangeary'

とするだけです。もしくは、特に手でインストールした場合は、
    require 'rangeary'

とする必要があるかも知れません。他のファイル(RangeExtd 用の `range_extd/range_extd.rb`)は、自動的に読込まれます。

{Rangeary} は Ruby 2.1 以上で動きます。

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

さらなる解説及び例は、{Rangeary.new}を参照して下さい。

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

ファイル `rangeary.rb` が読まれた段階で、RangeExtd ライブラリで 定義される二つのクラス(`RangeExtd` と
`RangeExtd::Infinity`)に加えて、クラス一つが定義されます。

*   Rangeary


### Rangeary クラス

{Rangeary} オブジェクトは、Range や RangeExtd と同様、イミュータブルで
す。だから、一度インスタンスが生成されると、変化しません。

インスタンスの生成方法は上述の通りです(「使用例」の章)。レンジとして"valid"と見 なされないインスタンスを生成しようとすると、すなわち
+Range#valid?+ が 偽を返すレンジを使おうとすると、例外(`ArgumentError`)が発生し、 失敗します。

なお、ユーザーは、{Rangeary} インスタンス生成の時、レンジ要素に対応し た正負の無限大オブジェクトを付加することができます。デフォルトでは、
`Float::INFINITY` または `RangeExtd::Infinity::POSITIVE` の類に なります。詳しくは
{Rangeary.new} を参照下さい。

生成時に複数のレンジが引数として与えられた時、ソートされて保持されます。 その時、要素のどこかに重複する部分があった時は、論理和として扱われます
(つまり、単純に足し合わされます)。これはつまり、{Rangeary} が内部的に 保持するオブジェクトは生成時に与えられたものとは異なる、すなわち
{#object_id} が異なるかも知れないことを意味します。特に、組込Rangeが引 数として与えられた時は、常に RangeExtd
オブジェクトに内部で変換されます。

もし生成時に与えられたレンジのどれかが空、すなわち +Range#empty?+ が真
を返す場合、それらは無視されます。ただし、引数の全てのレンジが空であっ た場合は、「最小」のものが残されます。

もし演算の結果として空の{Rangeary}が返される場合、その唯一の要素は `RangeExtd::NONE`となり、したがって
{Rangeary#empty_element?} が真を返します。

そのため、どの Rangeary オブジェクトも、{Rangeary#to_a}.size は常に正
の値を返し、零を返すことはありません。あるいは、Array から継承した {Rangeary#empty?}
は常に偽を返します(オブジェクトがレンジとして空かど うかをチェックするには、{Rangeary#empty_element?} を使って下さい)。
    Rangeary(RangeExtd::NONE).empty?          # => false
    Rangeary(RangeExtd::NONE).empty_element?  # => true

前述のように、ごく一部を除いた全ての Arrayクラスのメソッドがこの {Rangeary}
クラスに継承されていて、それらは各RangeExtd要素に対して 動作します。ただし、4個のメソッドの挙動が異なります。 {Rangeary#+} と
{Rangeary#*} とは、それぞれ {Rangeary#disjunction} と {Rangeary#conjunction}
とへのエイリアスとなっています。{Rangeary#===} は、全てのRangeExtd要素に対して+Range#===+を実行し、それらの一つでも
真を返せば真を返します。したがって、{Rangeary#===}(RangeExtd(**))は常 に偽を返します。 [また、](#length) と
[#reverse] とは未定義化されています。

{Rangeary#==} と {Rangeary#eql?} は、Arrayと同様に動作します。だから
    [2..4, 6..8] == Rangeary(2..4, 6..8)  # => true

も成り立ちます。しかし注意すべきは以下です。
    [2..4, 6..8] == Rangeary(6..8, 2..4)  # => true
    [6..8, 2..4] == Rangeary(6..8, 2..4)  # => false

端的には、標準Arrayとの直接比較は推奨しません。代わりに、 {Rangeary} オブジェクトと比較して下さい。

レンジ要素に対してではなく、レンジを構成する要素に対して動作する他の 全てのメソッドは、組込Arrayクラスに同名のメソッドが存在する場合、接尾辞
`_element` がつきます。たとえば、{Rangeary#size} は、保持する RangeExtd
オブジェクトの数を返し、一方、{Rangeary#size_element}は、 保持するすべての RangeExtdオブジェクトに対して
+Range#size+ を行った その総和を返します。
    Rangeary(1..3, 5..8).size          # => 2
    Rangeary(1..3, 5..8).size_element  # => 7

{Rangeary#flatten_element} は、全てのRangeExtd要素に対して {Rangeary#to_a}
を実行して、その結果を結合した配列を返します。

クラスとメソッドの完全なマニュアルは、 [Rubygems
のウェブサイト](http://rubygems.org/gems/rangeary)上にあります。
あるいは、ソースパッケージを展開して、ルートディレクトリで +make doc+ することで、手元でコンパイルすることもできます(RubyGems の
`yard` がインストールされている必要があります)。

### Array#==

等号メソッド +Rangeary#==+ は、Array とは異なった挙動をするべきであり、 実際します。まず、**empty?**
の際の意味が、異なります。なぜならば、 Rangeary オブジェクトは、Array的な意味では、empty になることは決してな
いからです。次に、Ruby 2.6で導入された [Endless
Range](https://rubyreferences.github.io/rubychanges/2.6.html#endless-range-1)
は、概念的にそう素直には組み込まれません。例えば、Ruby 2.6 では以下の ような挙動を示します。

    (?a..).size                  # => nil
    (10..).size                  # => Infinity
    (10..Float::INFINITY)).size  # => Infinity
    (1.0..) != (1.0..Float::INFINITY)

これらは、当たり前の挙動ではありませんし、若干矛盾があるとさえ言えるかも 知れません。Range#size
メソッドに関しては、Numericクラス(Complexを除く) のオブジェクトに関しては、Endless Range は、数学的無限のように振舞いま
す。しかし、両者を比較することはできず、等号比較すると、偽が返ります。

Rangeary は、複数の Range からなる数列(array)に対して、論理演算する機
能を提供します。そのためには、一貫した「無限」の定義が不可欠です。例え ば、 +[5..Infinity]+ の否定は、

    [-Infinity...5]

すなわち、無限小から始まり、5 で終わるが、終端は含まれない、ということ になります。これに対して再度否定演算を行うと、元の Rangeary
+(5..Infinity)+ が再度得られなければなりません。

この演算は、当然、+Endless Range+ だけでは不可能です。+Endless Range+
は正の無限大、あるいは正確には、正の方向に開いたもの、しか定義していないからです。 このため、`RangeExtd` を使用することは、Rangeary
では決定的に重要なの です。

Rangearyの演算(ここでは否定演算)においては、以下のようになります。

    r1 =   Rangeary(5..)   # => [(5..)]  (equivalent to (5..nil))
    r2 =  ~Rangeary(5..)   # => [(-Float::INFINITY...5)]
    r3 = ~~Rangeary(5..)   # => [(5..Float::INFINITY)]

    r4 =   Rangeary(?a..)  # => [(?a..)]  (equivalent to (?a..nil))
    r5 =  ~Rangeary(?a..)  # => [(RangeExtd::Infinity::NEGATIVE...?a)]
    r6 = ~~Rangeary(?a..)  # => [(?a..RangeExtd::Infinity::POSITIVE)]

この演算において、負の無限大も経過した最後の結果が、元々の出発点である(Ruby2.6で言う) Endless
Rangeにならなければいけない理由はないことになります。しかしながら、 +r1==r3+ および +r4==r6+ は成立すべきです。

+RangeExtd#==+ は、+Range#==+ とほぼ同一に振舞います(前者は `exclude_begin`
メソッドを持つことを除いて)。例えば、

             (1.0..) !=          (1.0..Float::INFINITY)
    RangeExtd(1.0..) != RangeExtd(1.0..Float::INFINITY)

+r1==r3+ や +r4==r6+ も成立するよう、数学的一貫性を保つために、Rangeary では、 等号の定義を変更する必要がありました。Array
クラスのデフォルトの標準等 号ではダメだからです。 そこで、Rangeary では、Endless Range と終端が何か別の無限大である
Rangeとは、他のパラメーター(例えば始端や`exclude_end`)が等しい限り、等 しいと見なします。

実装としては、本ライブラリ内で +Array#==+ (Rangearyに自然に継承される)を変更しています。 そのため、等号の可換性が保たれています。

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

### 無限大

正負の無限大は、Rangearyの演算に不可欠です。デフォルトでは、正負の一般無限大は `RangeExtd::Infinity::POSITIVE` と
`RangeExtd::Infinity::NEGATIVE` が使われます(詳細は
[RangeExtd](http://rubygems.org/gems/range_extd) 参照)。 但し、例外が、比較可能な Numerics
(Integer, Rational, Floatなど)で、 +Float::INFINITY がデフォルトで使われます。しかし、ユーザーは、オブジェ
クト作成の時、`positive:` および `negative:` で自分の無限大を定義することもできます。

なお、一旦、無限大が定義されると、そのオブジェクトと演算を行う全ての Rangearyオブジェクトも同じ無限大をもつべきです。デフォルトでは、演算す
るオブジェクト間でもし異なる無限大が使われている場合(端的には、一つの オブジェクトにだけ無限大を定義して他はデフォルトで済ませている場合)は、
正の無限大には最小の値、負の無限大には最大の値が選ばれます。但し、警告 メッセージが出るかもしれません(組込グローバル変数
+$VERBOSE+が真の時のみ)。 例外は、複数のRangearyから新しいRangeryオブジェクトを作成の際に、オプ
ションで陽に無限大を指定した時で、その時は、それらRangearyから継承した 値は考慮されず、指定した値が使われます。

上記の最後の例が、指定する例です。

## 既知のバグ

*   Ruby-2.7 で警告を出さないためには、[RangeExtd](https://rubygems.org/gems/range_extd)
    は、Ver.1.1.1 以上であること。
*   `Rangeary.new(-6..-5, 2..5, 8..8).last_element(3)` は、
        Ruby-2.6.x 以前は正しい値を返すが、Ruby-2.7 ではなぜか <tt>[3, 4, 5]</tt> と誤った値を返す。


このライブラリは Ruby 2.1 以上を必要とします。

パッケージに含まれている通り、網羅的なテストが実行されています。

## 未処理事項

特になし。

## 終わりに

このライブラリは、イアン・スチュワート氏が開発した、XMM-Newton望遠鏡用の SAS解析ソフトウェア・パッケージに含まれる(数値)複数レンジの
Fortran90ライブラリにアイデアを得たものです。彼の仕事に感謝します。

しかし、Rubyへの実装は、一筋縄ではいきませんでした。一つには組込Range クラスでは始点を除外することができないこと、また一つには Rubyでは数値
を除いて一般オブジェクトに対しての無限大が定義されていないからです。 小生の開発した RangeExtd によって初めてこのライブラリが可能となりま
した。これにより、今、比較可能な任意のオブジェクトについて、1次元上の レンジの完全性が実現できたことを嬉しく思います。

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

