import Math.Tensor
import Math.Tensor.Examples.Gravity

main :: IO ()
main = sequence_ $ map print $ toListShow6 interArea
