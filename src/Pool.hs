-- Copyright 2020 Google LLC
--
-- Licensed under the Apache License, Version 2.0 (the "License"); you may not
-- use this file except in compliance with the License. You may obtain a copy of
-- the License at
--
--     https://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
-- WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
-- License for the specific language governing permissions and limitations under
-- the License.

{-# OPTIONS_GHC -Wno-missing-import-lists #-}
module Pool                                                                                                           
  ( Pool                                                                                                              
  , empty, withReserved                                                                                               
  , null                                                                                                              
  , take, put                                                                                                         
  ) where                                                                                                             
                                                                                                                      
import Prelude hiding (null, take)                                                                                    
                                                                                                                      
import Control.Monad (guard)                                                                                          
import qualified Data.List as List                                                                                    
import Data.Set (Set)                                                                                                 
import qualified Data.Set as Set                                                                                      
                                                                                                                      
data Pool a = Pool { reserved :: Set a, taken :: Set a } deriving (Eq, Show)                                          
                                                                                                                      
empty :: Pool a                                                                                                       
empty = withReserved Set.empty                                                                                        
                                                                                                                      
withReserved :: Set a -> Pool a                                                                                       
withReserved xs = Pool { reserved = xs, taken = Set.empty }                                                           
                                                                                                                      
null :: Pool a -> Bool                                                                                                
null p = Set.null (taken p)                                                                                           
                                                                                                                      
take :: (Bounded a, Enum a, Ord a) => Pool a -> Maybe (a, Pool a)                                                     
take p = do                                                                                                           
  x <- nextAvailable p                                                                                                
  return (x, p { taken = Set.insert x (taken p) })                                                                    
                                                                                                                      
nextAvailable :: (Bounded a, Enum a, Ord a) => Pool a -> Maybe a                                                      
nextAvailable p = List.find (`Set.notMember` unavailable p) (enumFrom minBound)                                       
                                                                                                                      
unavailable :: (Ord a) => Pool a -> Set a                                                                             
unavailable p = reserved p `Set.union` taken p                                                                        
                                                                                                                      
put :: Ord a => a -> Pool a -> Maybe (Pool a)                                                                         
put x p = do                                                                                                          
  guard $ x `Set.member` taken p                                                                                      
  return $ p { taken = Set.delete x (taken p) }                                                                       
