# testthat test script for invquad.R
# 
# Author: Paul Hurley
###############################################################################
context("Finding the Roots of a Quadratic Equation")
test_that("Simple Quadratics Match worked results",{
			expect_that(invquad(1,0,-1,0),equals(c(1,-1)), "Roots of y=x^2-1 should be 1 and -1")
			#expect_that(invquad(0,1,-1,0),equals(c(1,NA)), "Roots of y=x-1 should be 1 and NA")
			expect_that(invquad(1,-2,1,0),equals(c(1,1)), "Roots of y=-2x+x^2+1 should both be 1")
			expect_that(invquad(2,6,-3,0),equals(c((-3/2)+sqrt(15/4),(-3/2)-sqrt(15/4))), "Roots of y=2x^2+6x-3 should be -3/2 +/- sqrt(15/4)")
			expect_that(invquad(1,-2,-2,0),equals(c(1+sqrt(3),1-sqrt(3))), "Roots of y=-2x+x^2-2 should be 1+sqrt(3) and 1-sqrt(3)")
			#expect_that(1,equals(2),"This should fail")
		})
test_that("Tests with xmin and xmax limits",{
			expect_that(invquad(0.5,0.005,-0.2,50, xmin=0, xmax=200, na.rm=TRUE),takes_less_than(5))
			expect_that(round(invquad(0.005,0.05,-0.2,50, xmin=0, xmax=200, na.rm=TRUE),digits=5),
					equals(95.32447))
			#expect_that(1,equals(2),"and this one will fail")
		})


