MetaRanking_custom <- function (decision, weights, cb, lambda, v, AB, CD) 
{
  MMoora = MMOORA(decision, weights, cb)
  TopsisV = TOPSISVector(decision, weights, cb)
  TopsisL = TOPSISLinear(decision, weights, cb)
  Vikor = VIKOR(decision, weights, cb, v)
  Waspas = WASPAS(decision, weights, cb, lambda)
  if (Vikor[1, 5] == "-") {
    MetaR = MMoora[, 8] + TopsisV[, 3] + TopsisL[, 
                                                            3] + Waspas[, 5]
  }
  else {
    MetaR = MMoora[, 8] + TopsisV[, 3] + TopsisL[, 
                                                            3] + Vikor[, 5] + Waspas[, 5]
  }
  if (Vikor[1, 5] == "-") {
    ra = rbind(MMoora[, 8], TopsisV[, 3], TopsisL[, 
                                                            3], Waspas[, 5])
  }
  else {
    ra = rbind(MMoora[, 8], TopsisV[, 3], TopsisL[, 
                                                            3], Vikor[, 5], Waspas[, 5])
  }
  if (nrow(decision) <= 10) {
    RA = RankAggreg::BruteAggreg(ra, nrow(decision), distance = "Spearman")
  }
  else {
    RA = RankAggreg::RankAggreg(ra, nrow(decision), method = "GA", 
                                distance = "Spearman", verbose = FALSE)
  }
  return(data.frame(Alternatives = 1:nrow(decision), MMOORA = MMoora[, 
                                                                     8], TOPSISVector = TopsisV[, 3], TOPSISLinear = TopsisL[, 
                                                                                                                                             3], VIKOR = Vikor[, 5], WASPAS = Waspas[, 5], MetaRanking_Sum = rank(MetaR, 
                                                                                                                                                                                                                  ties.method = "first"), MetaRanking_Aggreg = RA$top.list))
}
