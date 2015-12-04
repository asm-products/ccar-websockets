

# While the rest of the world uses + to concatenate, 
# Julia has to be different: *. what else are we going to see.

function binomial_rv(n, p)
	println("Binomial varible " * string(n) *  " with " * string(p))
	results = randn(n)
	results = results [ results .> 0]
	results = results [ results .< 1]
	results = results [results .< p]
end


# Some more isssues with the language,
# Unexpected mutability. Specially when arrays are shallow
# copied.

println(binomial_rv(100, 0.5))