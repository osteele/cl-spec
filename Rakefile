FILES = Dir['*'] - %w{agenda.txt bdd-example.lisp Rakefile tests.rb outtakes.lisp} - Dir['#*'] - Dir['*.fasl']

task :publish do
  sh "rsync -vzP #{FILES.join(' ')} osteele.com:osteele.com/sources/lisp/cl-spec"
end
