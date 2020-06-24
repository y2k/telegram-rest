(defn getLastMessages [resolveUsername getHistory r]
  (-!>
    (d/parseString 32 r)
    (resolveUsername)
    (unwrap (cantFindChat response))
    (getHistory 50)
    (d/toSnapshots)
    (r/toJsonResponse)
    ))