
-type purge_options() :: #{what := all | keys | time, data => term()}.

-type doc_options() :: #{key => term(), rebuild => boolean()}.


-record (mtag,{path :: string(), mod :: integer(), size :: integer()}).


