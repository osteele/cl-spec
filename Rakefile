FILES = Dir['*'] - %w{agenda.txt bdd-example.lisp Rakefile tests.rb outtakes.lisp} - Dir['#*'] - Dir['*.fasl']

task :publish do
  sh "rsync -avzP #{FILES.join(' ')} osteele.com:osteele.com/sources/lisp/cl-spec --delete"
end

task :staging do
  sh "rsync -avzP #{FILES.join(' ')} ~/Sites/osteele.com/sources/lisp/cl-spec"
end
