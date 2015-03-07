isNotTrivialCurious :: Int -> Int -> Bool
isNotTrivialCurious teller noemer = curious && notTrivial
  where
    curious = (((fromIntegral (read ([head (show teller)])))
              / (fromIntegral (read ([last (show noemer)])))) == ((fromIntegral teller) / (fromIntegral noemer))
              || ((fromIntegral (read ([last (show teller)])))
              / (fromIntegral (read ([head (show noemer)])))) == ((fromIntegral teller) / (fromIntegral noemer)))
              && (((fromIntegral (read ([head (show teller)]))) == (fromIntegral (read ([last (show noemer)])))
              || ((fromIntegral (read ([last (show teller)]))) == (fromIntegral (read ([head (show noemer)]))))))

    notTrivial = (tail (show teller)) /= "0" && (tail (show noemer)) /= "0" && (head (show teller)) /= (last (show teller))
                 && (head (show noemer)) /= (last (show noemer))

main = print $ foldl (\(x, y) (cx, cy) -> (x*cx, y*cy)) (1, 1) $ [(teller, noemer) | teller <- [10..99], noemer <- [10..99], teller < noemer, isNotTrivialCurious teller noemer]
