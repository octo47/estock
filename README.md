estock
======

Toy Erlang project

EStock server handles information about stock prices.
Input consists of

	row {
		name,
		time,	
		price,
		amount
	}

EStock aggregates by configured scales: 

	minute
	hour
	day
	week
	month
	year

Each aggregate stored in ets table with key { Name, Scale, StartOfScale }.
For each aggregate stored:

	agg {
		open_price,
		open_price_timestamp,
		close_price,
		close_price_timestamp,
		min_price,
		max_price,
		amount
	}


In distributed mode each node has separate ets table and stores
it own aggregates. When client queries aggregates server invokes
in parallel requests to each node, where Namecan be found.

Siege results can be found here: https://github.com/octo47/estock/tree/master/priv/siege/results