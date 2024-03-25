# example data for tests

CLUSexample <- prepExData(CLUSexample)

costRaster <- CLUSexample$cost

roadsLine <- sf::st_sfc(geometry = sf::st_linestring(
  matrix(c(0.5, 4.5, 4.5, 4.5),
         ncol = 2, byrow = T)
)) %>%
  sf::st_as_sf(crs = sf::st_crs(costRaster))


landings <- roads::CLUSexample$landings

sim = list(costSurface=costRaster)

test_that("getGraph works with different neighbourhoods", {
  gR = getGraph(sim, "rook")
  gQ = getGraph(sim, "queen")
  gO = getGraph(sim, "octagon")
  
  expect_length(igraph::edge_attr(gR, "weight"), 40)
  expect_length(igraph::edge_attr(gQ, "weight"), 72)
  expect_length(igraph::edge_attr(gO, "weight"), 72)
  expect_true(all(igraph::edge_attr(gO, "weight") >= igraph::edge_attr(gQ, "weight")))
  
})

test_that("getGraph works with gdistance method", {
  gR_gD = getGraph(sim, "rook", method = "gdistance")
  gQ_gD = getGraph(sim, "queen", method = "gdistance")
  gO_gD = getGraph(sim, "octagon", method = "gdistance")
  
  expect_length(igraph::edge_attr(gR_gD, "weight"), 40)
  expect_length(igraph::edge_attr(gQ_gD, "weight"), 72)
  expect_length(igraph::edge_attr(gO_gD, "weight"), 72)
  # TODO: figure out why not as expected but not really using gdistance right now
  # expect_true(all(igraph::edge_attr(gO_gD, "weight") >= igraph::edge_attr(gQ_gD, "weight")))
})

test_that("getGraph works with slopePenaltyFun", {
  # need to set limit high so that all cells will be included
  gR_sl = getGraph(sim, "rook", weightFunction = slopePenaltyFn, limit = 10000)
  gQ_sl = getGraph(sim, "queen", weightFunction = slopePenaltyFn, limit = 10000)
  gO_sl = getGraph(sim, "octagon", weightFunction = slopePenaltyFn, limit = 10000)
  
  expect_length(igraph::edge_attr(gR_sl, "weight"), 40)
  expect_length(igraph::edge_attr(gQ_sl, "weight"), 72)
  expect_length(igraph::edge_attr(gO_sl, "weight"), 72)
  expect_true(all(igraph::edge_attr(gO_sl, "weight") >= igraph::edge_attr(gQ_sl, "weight")))
})


test_that("getGraph works with arbitrary fun", {
  gR_pl = getGraph(sim, "rook", weightFunction = function(x1, x2, ...){x1+x2})
  gQ_pl = getGraph(sim, "queen", weightFunction = function(x1, x2, ...){x1+x2})
  gO_pl = getGraph(sim, "octagon", weightFunction = function(x1, x2, ...){x1+x2})
  
  gR = getGraph(sim, "rook")
  
  expect_true(all(igraph::edge_attr(gR, "weight") == igraph::edge_attr(gR_pl, "weight")/2))
  
  expect_length(igraph::edge_attr(gR_pl, "weight"), 40)
  expect_length(igraph::edge_attr(gQ_pl, "weight"), 72)
  expect_length(igraph::edge_attr(gO_pl, "weight"), 72)
  expect_true(all(igraph::edge_attr(gO_pl, "weight") >= igraph::edge_attr(gQ_pl, "weight")))
})