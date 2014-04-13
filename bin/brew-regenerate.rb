#! /usr/bin/env ruby
# Outputs brew install commands that can be used to re-install all packages
# Place brew-regenerate on the PATH
# run: brew regenerate

require 'formula'
require 'tab'

# To debug with pry install the gem into the Ruby 1.8 installed with the system.
# /System/Library/Frameworks/Ruby.framework/Versions/1.8/usr/bin/ruby
#
# If using rvm switch to the system ruby:
#
#   $ rvm system
#
# In OS 10.9 the default Ruby is: ruby-2.0.0-p247.
#
#   $ cd /System/Library/Frameworks/Ruby.framework/Versions
#   $ ls -l
#   total 16
#   drwxr-xr-x  7 root  wheel  238 Nov  4 17:24 1.8
#   drwxr-xr-x  7 root  wheel  238 Nov  4 17:28 2.0
#   lrwxr-xr-x  1 root  wheel    3 Nov  4 19:15 Current -> 2.0
#
# Install pry into Ruby 1.8 installed with the system:
#
#   $ sudo /System/Library/Frameworks/Ruby.framework/Versions/1.8/usr/bin/gem install pry
#
# Add this to the brew ruby code where you want to debug:
#
#   require 'rubygems'
#   require 'pry'
#
# And add this where you want to break
#
#   binding.pry

def get_used_by(formulae)
  used_by = {}
  formulae.each do |f|
    f.active_spec.deps.each do |dep|
      _deps = used_by[dep.to_s] || { :required => [], :optional => [] }
      if dep.optional?
        _deps[:optional] << f.name unless _deps[:optional].include? f.name
      else
        _deps[:required] << f.name unless _deps[:required].include? f.name
      end
      used_by[dep.to_s] = _deps
    end
  end
  return used_by
end

installed = Formula.installed
names = installed.map(&:name)
used_by_map = get_used_by(installed)

root_formulae = []

names.each do |name|
  used_by = used_by_map[name]
  if used_by && used_by[:required].empty?
    root_formulae << installed.find { |f| f.name == name }
  elsif !used_by
    root_formulae << installed.find { |f| f.name == name }
  end
end

root_formulae.sort! { |f1, f2|
  f1_deps = f1.active_spec.deps.map(&:name)
  f2_deps = f2.active_spec.deps.map(&:name)
  # binding.pry
  if f1_deps.include? f2.name
    1
  elsif f2_deps.include? f1.name
    -1
  else
    0
  end
}

# ffmpeg installed as follows
#   brew install ffmpeg --devel --with-libvpx --with-libvorbis
# devel version is installed:
#  /usr/local/bin/ffmpeg -> ../Cellar/ffmpeg/2.0.2/bin/ffmpeg
# But ffmpeg.active_spec == ffmpeg.devel
# ffmpeg = installed.find { |f| f.name == "ffmpeg" }
# ffmpeg.active_spec == ffmpeg.devel => false

# to break here with pry
# binding.pry

root_formulae.each do |f|
  name = f.name
  flags = ""
  if f.active_spec == f.devel
    flags = "--devel"
  elsif f.active_spec == f.head
    flags = "--HEAD"
  end
  tab = Tab.for_name(name)
  if tab.used_options.empty?
    options = ""
  else
    options = tab.used_options.to_a.join(' ')
  end
  puts "brew install -v #{name} #{flags} #{options}"
end
