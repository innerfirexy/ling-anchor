# experiment with Julia computing the cosine distance
# Yang Xu
# 1/26/2016

using DataFrames, DataArrays

# read csv
df_pairs = readtable("wiki.pairs.csv")


# define the func for computing cosine distance
function cosDist(x, y)
    1 - dot(x, y) / (norm(x) * norm(y))
end

# compute dist
function exp1(df)
    dists = []
    for row in eachrow(df)
        x = vec(convert(Array, row[7:14]))
        y = vec(convert(Array, row[20:27]))

        println(x)
        println(y)

        if all(x == zeros(8))
            x .+ 0.1
        end
        if all(y == zeros(8))
            y .+ 0.1
        end

        dist = cosDist(x, y)
        push!(dists, dist)
    end
    collect(dists)
end