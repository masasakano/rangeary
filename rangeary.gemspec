# -*- encoding: utf-8 -*-

Gem::Specification.new do |s|
  s.name = %q{rangeary}
  s.version = "0.1.0"
  # s.required_rubygems_version = Gem::Requirement.new(">= 0") if s.respond_to? :required_rubygems_version=
  # s.executables << 'hola'
  # s.bindir = 'bin'
  s.authors = ["Masa Sakano"]
  s.date = %q{2014-05-09}
  s.summary = %q{Rangeary - class to represent any 1-dimensional multiple-range}
  s.description = %q{Rangeary is a sub-class of Array and represents any 1-dimensional multiple-range.  For example, (x<3 and 7<x<=9).  All the standard logical operations, such as negation and conjunction, are supported and can be used with conventional Ruby-style operators.  Each range is represented as RangeExtd class (Extended Range), which is a sub-class of Range and supports exclude-begin and open-ended (to Infinity) ranges, and is downloadable from https://rubygems.org/gems/range_extd}
  # s.email = %q{abc@example.com}
  s.extra_rdoc_files = [
    # "LICENSE",
     #"README.en.rdoc",
     "README.ja.rdoc",
  ]
  s.license = 'MIT'
  s.files = [
    #".document",
     #".gitignore",
     #"VERSION",
     "News",
     "ChangeLog",
     #"README.en.rdoc",
     "README.ja.rdoc",
     "Rakefile",
     "rangeary.gemspec",
     "lib/rangeary/rangeary.rb",
     "test/test_rangeary.rb",
  ]
  # s.add_runtime_dependency 'library', '~> 2.2', '>= 2.2.1'	# 2.2.1 <= Ver < 2.3.0
  # s.add_development_dependency "bourne", [">= 0"]
  # s.homepage = %q{http://}
  s.rdoc_options = ["--charset=UTF-8"]
  # s.require_paths = ["lib"]
  s.required_ruby_version = '>= 2.0'
  s.test_files = [
     "test/test_rangeary.rb",
  ]
  # s.test_files = Dir.glob('test/tc_*.rb')
  # s.requirements << 'libmagick, v6.0'	# Simply, info to users.
  # s.rubygems_version = %q{1.3.5}	# This is always set automatically!!

end

