# -*- encoding: utf-8 -*-

require 'rake'
require 'date'

Gem::Specification.new do |s|
  s.name = %q{rangeary}.sub(/.*/){|c| (c == File.basename(Dir.pwd)) ? c : raise("ERROR: s.name=(#{c}) in gemspec seems wrong!")}
  s.version = "1.0"
  # s.required_rubygems_version = Gem::Requirement.new(">= 0") if s.respond_to? :required_rubygems_version=
  # s.executables << 'hola'
  # s.bindir = 'bin'
  s.authors = ["Masa Sakano"]
  s.date = %q{2019-11-01}.sub(/.*/){|c| (Date.parse(c) == Date.today) ? c : raise("ERROR: s.date=(#{c}) is not today!")}
  s.summary = %q{Rangeary - class to represent any 1-dimensional multiple-range}
  s.description = %q{Rangeary is a sub-class of Array and represents any 1-dimensional multiple-range.  For example, (x<4 and 7<x<=9).  All the standard logical operations, such as negation and conjunction, are supported and can be used with conventional Ruby-style operators.  Each range is represented as RangeExtd class (Extended Range), which is a sub-class of Range and supports exclude-begin and open-ended (to Infinity) ranges, and is downloadable from https://rubygems.org/gems/range_extd}
  # s.email = %q{abc@example.com}
  s.extra_rdoc_files = [
    # "LICENSE",
     #"README.en.rdoc",
     "README.ja.rdoc",
  ]
  s.license = 'MIT'
  s.files = FileList['.gitignore','lib/**/*.rb','[A-Z]*', 'test/**/*.rb'].to_a.delete_if{ |f|
    ret = false
    arignore = IO.readlines('.gitignore')
    arignore.map{|i| i.chomp}.each do |suffix|
      if File.fnmatch(suffix, File.basename(f))
        ret = true
        break
      end
    end
    ret
  }
  s.files.reject! { |fn| File.symlink? fn }

  s.add_runtime_dependency 'range_extd', '>= 1.1'	# Range#equiv? introduced in 0.4.0
  # s.add_runtime_dependency 'library', '~> 2.2', '>= 2.2.1'	# 2.2.1 <= Ver < 2.3.0
  # s.add_development_dependency "bourne", [">= 0"]
  s.homepage = %q{https://www.wisebabel.com}
  s.rdoc_options = ["--charset=UTF-8"]

  # s.require_paths = ["lib"]
  s.required_ruby_version = '>= 2.1'
  s.test_files = Dir['test/**/*.rb']
  s.test_files.reject! { |fn| File.symlink? fn }
  # s.requirements << 'libmagick, v6.0'	# Simply, info to users.
  # s.rubygems_version = %q{1.3.5}	# This is always set automatically!!

  s.metadata["yard.run"] = "yri" # use "yard" to build full HTML docs.
end

