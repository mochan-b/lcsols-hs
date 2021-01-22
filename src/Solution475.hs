module Solution475
    ( findHeater, findRadius
    ) where

import qualified Data.List

findHeater :: Int -> [Int] -> Int 
findHeater house heaters = findHeaterHelp 0 (length heaters - 1) 
    where 
        findHeaterHelp :: Int -> Int -> Int 
        findHeaterHelp left right = do
            let mid = div (left + right) 2
            if left > right
                then left
            else
                if heaters!!mid == house
                    then mid 
                else 
                    if heaters!!mid < house 
                        then findHeaterHelp (mid+1) right 
                    else
                        findHeaterHelp left (mid-1)

    
findRadius :: [Int] -> [Int] -> Int 
findRadius houses heaters = maximum (map heaterDist houses)        
    where        
        heaterDist :: Int -> Int 
        heaterDist house = do            
            let sortedHeaters = Data.List.sort heaters
            let heaterIndex = findHeater house sortedHeaters
            let leftDistFn = house - sortedHeaters!!(heaterIndex - 1)
            let rightDistFn = sortedHeaters!!heaterIndex - house
            if heaterIndex == 0
                then rightDistFn
            else
                if heaterIndex == length heaters
                    then leftDistFn
                else
                    min leftDistFn rightDistFn