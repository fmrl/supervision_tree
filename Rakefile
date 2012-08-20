# $legal:1594:
# 
# Copyright (c) 2011, Michael Lowell Roberts.  
# All rights reserved. 
# 
# Redistribution and use in source and binary forms, with or without 
# modification, are permitted provided that the following conditions are 
# met: 
# 
#   - Redistributions of source code must retain the above copyright 
#   notice, this list of conditions and the following disclaimer. 
# 
#   - Redistributions in binary form must reproduce the above copyright 
#   notice, this list of conditions and the following disclaimer in the 
#   documentation and/or other materials provided with the distribution.
#  
#   - Neither the name of the copyright holder nor the names of 
#   contributors may be used to endorse or promote products derived 
#   from this software without specific prior written permission. 
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS 
# IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED 
# TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A 
# PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER 
# OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED 
# TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR 
# PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF 
# LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING 
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS 
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
# 
# ,$

require 'rubrstmp/rake_tasks'

# [mlr][todo] abstract erlang and rebar-specific things into separate
# rakefile.
namespace :rebar do
   REBAR = ENV['REBAR'] || 'bin/rebar'
   task :compile do
      desc "compile erlang sources."
      sh "#{REBAR} compile"
   end
   task :clean do
      desc "clean erlang build."
      sh "#{REBAR} clean"
   end
   task :doc do
      desc "build edoc documentation."
      sh "#{REBAR} doc"
   end
end

task :default => 'rebar:compile' do
end

ESHELL = ENV['ESHELL'] || 'bin/eshell'
desc "invoke the erlang shell."
task :eshell => 'rebar:compile' do
   sh ESHELL
end

namespace :rubrstmp do
   exclude \
      '*.app', 
      '*.beam', 
      '*.md', 
      'c_priv/**',
      'doc/**', 
      'vendor/**' 
   file_keywords \
      'legal' => 'LICENSE.md',
      'vim' => 'etc/rubrstmp/vim/default',
      'vim-c' => 'etc/rubrstmp/vim/c',
      'vim-erl' => 'etc/rubrstmp/vim/erlang',
      'vim-rb' => 'etc/rubrstmp/vim/ruby'
end

# $vim-rb:31: vim:set sts=3 sw=3 et ft=ruby:,$
