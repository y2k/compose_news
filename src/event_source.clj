(defn- compute_object_hash [obj]
  (defn- sha256 [data]
    (crypto.subtle.digest "SHA-256" data))
  (->
   (TextEncoder.)
   (.encode (JSON.stringify obj))
   sha256
   (.then (fn [data]
            (-> data Buffer.from (.toString "base64"))))))

(defn- query_serialized_fx [env key]
  (->
   (.prepare env.DB "SELECT content FROM log WHERE id = ?")
   (.bind key)
   (.first "content")
   (.then (fn [r]
            (if (some? r)
              {:result (:out (JSON.parse r))}
              nil)))))

(defn- handle_effect [env fx_name base_fx args]
  (let [key {:effect fx_name :in args}]
    (.then
     (compute_object_hash key)
     (fn [key_hash]
       (.then
        (query_serialized_fx env key_hash)
        (fn [serialized_fx]
          (if (some? serialized_fx)
            (:result serialized_fx)
            (.then
             (base_fx args)
             (fn [fx_result]
               (->
                (.prepare env.DB "INSERT INTO log (id, content) VALUES (?, ?)")
                (.bind key_hash (JSON.stringify {:effect fx_name :date (Date.) :out fx_result}))
                (.run)
                (.then (fn [] fx_result))))))))))))

(defn decorate [env]
  (.reduce
   (Object.entries env)
   (fn [acc [k v]]
     (assoc acc k (fn [args] (handle_effect (:bindings env) k v args))))
   env))

(defn reset_cache [env]
  (.exec env.DB "DELETE FROM log"))
