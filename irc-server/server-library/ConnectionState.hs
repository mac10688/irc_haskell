module ConnectionState (
    User,
    Address,
    ConnectionState,
    lookupByUser,
    insertUser,
    removeUser,
    lookupByAddress,
    insertSocketAddress,
    removeSocketAddress
) where

import Prelude hiding (lookup)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Control.Monad (join)

newtype User = User String deriving (Eq, Ord)
newtype Address = Address String deriving (Eq, Ord)

data ConnectionState = ConnectionStatePrivate {
     addrToUsrMap :: M.Map Address (Maybe User)
    ,usrToAddrsMap :: M.Map User (S.Set Address)
}

lookupByUser :: ConnectionState -> User -> Maybe (S.Set Address)
lookupByUser cs usr = M.lookup usr (usrToAddrsMap cs)

insertUser :: ConnectionState -> Address -> User -> ConnectionState
insertUser cs addr usr = 
    let 
        newAddressToUserMap = M.insert addr (Just usr) $ addrToUsrMap cs
        ipSetForUser = S.insert addr $ fromMaybe S.empty $ M.lookup usr $ usrToAddrsMap cs
        newUsrMap = M.insert usr ipSetForUser $ usrToAddrsMap cs
    in
        ConnectionStatePrivate newAddressToUserMap newUsrMap

removeUser :: ConnectionState -> User -> ConnectionState
removeUser cs usr = 
    let
        addrSet = fromMaybe S.empty $ M.lookup usr $ usrToAddrsMap cs
        newAddrMap = foldl (\m a -> M.delete a m ) (addrToUsrMap cs) addrSet
        newUsrMap = M.delete usr $ usrToAddrsMap cs
    in
        ConnectionStatePrivate newAddrMap newUsrMap

lookupByAddress :: ConnectionState -> Address -> Maybe User
lookupByAddress cs addr = join $ M.lookup addr (addrToUsrMap cs)

insertSocketAddress :: ConnectionState -> Address -> ConnectionState
insertSocketAddress cs addr =
    let
        newAddrMap = M.insert addr Nothing $ addrToUsrMap cs
    in
        ConnectionStatePrivate newAddrMap (usrToAddrsMap cs)

removeSocketAddress :: ConnectionState -> Address -> ConnectionState
removeSocketAddress cs addr =
    let
        maybeUser = join $ M.lookup addr $ addrToUsrMap cs
        newAddrMap = M.delete addr $ addrToUsrMap cs
        newUserMap = case maybeUser of
                            Just usr ->
                                let 
                                    newAddrSet = S.delete addr $ fromMaybe S.empty $ M.lookup usr (usrToAddrsMap cs)
                                in
                                    M.insert usr newAddrSet (usrToAddrsMap cs)
                            Nothing -> usrToAddrsMap cs
    in
        ConnectionStatePrivate newAddrMap newUserMap
                                



