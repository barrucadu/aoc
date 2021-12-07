import Common
import Utils

main :: IO ()
main = mainFor 7 parse (show . solve)

{-
The ideal solution is within 0.5 of the mean position

First, the cost for a single crab moving from position `p` to a target
position `t`:

\DeclareMathOperator{\cost}{cost}

\begin{align*}
  \cost(p, t) &= \sum^{|p - t|}_{i=1} i \\
              &= \frac{|p - t| \times \left(|p - t| + 1\right)}{2}
\end{align*}

Using the usual sum-from-1-to-n formula.

So, the cost for all crabs moving from positions `p_0`, `p_1`, `p_2`,
... to a target position `t` is the sum over all such costs for `p_i`:

\DeclareMathOperator{\totalcost}{totalcost}
\DeclareMathOperator{\sign}{sign}

\begin{align*}
  \totalcost(P, t) &= \sum_{p_i \in P} \cost(p_i, t) \\
                   &= \sum_{p_i \in P} \left( \frac{|p_i - t| \times \left(|p_i - t| + 1\right)}{2} \right) \\
                   &= \frac{1}{2} \sum_{p_i \in P} \left( |p_i - t| \times \left(|p_i - t| + 1\right) \right)
\end{align*}

We want to find a minimum of this function, so first let's take its
derivative (the derivative of the abs function is the sign function),
and find the value of `t` where the derivative is zero (ie, it's
reached a minimum or maximum):

Furthermore, let `n` be the number of crabs.

\begin{align*}
  \frac{dy}{dt} &= \frac{1}{2} \sum_{p_i \in P} \left( 2(p_i - t) + \sign(p_i - t) \right) \\
  0             &= \frac{1}{2} \sum_{p_i \in P} \left( 2(p_i - t) + \sign(p_i - t) \right) \\
                &= \frac{1}{2} \left(\sum_{p_i \in P} 2p_i - \sum_{p_i \in P} 2t \right + \sum_{p_i \in P} \sign(p_i - t) \right) \\
                &= \sum_{p_i \in P} p_i - \sum_{p_i \in P} t \right + \frac{1}{2}\sum_{p_i \in P} \sign(p_i - t) \\
                &= \sum_{p_i \in P} p_i - nt + \frac{1}{2}\sum_{p_i \in P} \sign(p_i - t) \\
  nt            &= \sum_{p_i \in P} p_i - \frac{1}{2}\sum_{p_i \in P} \sign(p_i - t) \\
  t             &= \frac{1}{n}\sum_{p_i \in P} p_i - \frac{1}{2n}\sum_{p_i \in P} \sign(p_i - t)
\end{align*}

So when the derivative is zero, `t` is the average position minus some
awkward fraction involving the `sign` function.

To get the maximum, let's say `p_i` is always greater than `t`: then
the `sign` function is always 1, and so the result is `n / 2n = 0.5`.

To get the minimum, let's say `p_i` is always less than `t`: then the
result is `-0.5`, similarly.

So when the derivative is zero, `t` is the mean plus or minus a half.
So, we can take the integral mean and check the three positions `[mean
- 1, mean, mean + 1]` to get the best.

-}
solve :: [Int] -> Int
solve positions = minimum (map cost candidates) where
  cost target = sum $ map (\pos -> sumFromOneToN $ abs (pos - target)) positions
  sumFromOneToN n = (n * (n + 1)) `div` 2

  candidates =
    let mean = sum positions `div` length positions
    in [mean - 1, mean, mean + 1]
