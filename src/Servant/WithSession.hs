
module Servant.WithSession where



import           Web.Cookie
import           Protolude
import           GHC.TypeLits
import           Data.UUID
import           Servant.API
import           Servant.Server
import           Servant.Server.Internal.RoutingApplication
import           Network.Wai.Internal
import           System.Random
import           Data.Binary.Builder
import           Network.HTTP.Types.Header                   (ResponseHeaders)
import           Servant.Server.Internal.Router
import           Data.Profunctor

data WithSession (name :: Symbol)



instance 
       ( KnownSymbol cookie
       , HasServer api (cont::[*])

       ) => HasServer (WithSession cookie :> api) (cont::[*]) 
   where

    type ServerT (WithSession cookie :> api) m = UUID -> ServerT api m 


    route _ ctx server = settingCookie name <$> route pSub ctx (withCookie name server)

      where
        pSub       = Proxy :: Proxy api
        name       = toSL $ symbolVal (Proxy :: Proxy cookie)


--addHeaders :: ResponseHeaders ->  Router env -> Router env
--addHeaders headers route = undefined

{-
   TODO: - add response
         - add cookie to the request!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-}

-- TODO: refactor
-- also add requests header
-- TODO: factor this out into a library...

settingCookie :: Text -> RoutingApplication -> RoutingApplication
settingCookie c original req cont = do let (cookies, content) = extract c req
                                       case content of
                                        Just _  -> original  req  cont
                                        Nothing -> do uuid    <- randomIO :: IO UUID
                                                      let val = show uuid
                                                      original (withCookies ( (toSL c,val) : cookies)) 
                                                               (cont' val)
  where
    
    cont'  uuid = lmap (fmap $ addSetCookie uuid) cont

    addSetCookie :: ByteString -> Response -> Response
    addSetCookie val = replaceHeader 
                          ( "Set-Cookie" 
                          , toSL . toLazyByteString . renderSetCookie
                          $ def{ setCookieName     = toSL c
                               , setCookieValue    = val
                               , setCookieHttpOnly = True
                               , setCookieSecure   = True
                               }
                          )

    cookiesList x = ( "Cookie"
                    , toSL . toLazyByteString $ renderCookies x
                    )

    withCookies x = req{ requestHeaders = (cookiesList x) 
                                        : (filter ((/="Cookie").fst) $ requestHeaders req)
                       }



extract :: Text -> Request -> (Cookies, Maybe UUID) 
extract name req = let cookie_ = find ((==name').fst)
                               $ cookies  

                    in (cookies, readMaybe . toSL . snd =<< cookie_)

   where
    cookies     = maybe [] (parseCookies.snd)
                . find ((=="Cookie").fst) 
                $ requestHeaders req
    
    name'       = toSL name :: ByteString

-- TODO: after the PR, use the one from wai util
mapHeaders :: (ResponseHeaders -> ResponseHeaders) -> Response -> Response
mapHeaders f (ResponseFile s h b1 b2) = ResponseFile s (f h) b1 b2
mapHeaders f (ResponseBuilder s h b)  = ResponseBuilder s (f h) b
mapHeaders f (ResponseStream s h b)   = ResponseStream s (f h) b
mapHeaders f (ResponseRaw io resp)   = ResponseRaw io (mapHeaders f resp)


addHeader h = mapHeaders (\hs -> h:hs)

-- | Set the matching header name to this in a 'Response'

replaceHeader h = mapHeaders (replaceHeader' h)

-- | Set the matching header name to this in 'ResponseHeaders'

replaceHeader' (n, v) = ((n,v):) . filter ((/=n) . fst)





withCookie :: Text -> Delayed env ( UUID -> c ) ->  Delayed env  c 
withCookie c Delayed{..} = Delayed capturesD methodD authD' bodyD serverD'
 
 where

    authD' = (,) <$> DelayedIO ( return
                               . maybe (FailFatal err500) Route 
                               . snd . extract c
                               )
                 <*> authD

    serverD' x (uuid,y) z req = ($ uuid) <$> serverD x y z req



