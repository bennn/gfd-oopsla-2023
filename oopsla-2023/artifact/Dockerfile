FROM racket/racket:8.8-full
RUN apt-get update -y
RUN apt-get install -y apt-utils build-essential vim libedit-dev git
RUN raco pkg install --auto lang-file-lib lang-file ppict basedir rackunit-abbrevs pict-abbrevs with-cache gtp-util text-table
