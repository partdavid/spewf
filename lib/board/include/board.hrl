-record(post,
        {id,
         timestamp,
         author,
         parent,
         summary,
         text}).

-record(user,
        {id,
         nickname,
         password,
         email,
         friends,
         watching,
         text}).
