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
They was made on my notebook, 4Gb RAM, SSD, Core i5 (4 cores).

<table>
<tr>
	<th>test</th><th>smp, rps</th><th>no-smp, rps</th>
</tr>
<tr>
	<td>add</td><td>4415.01</td><td>4238.99</td>
</tr>
<tr>
	<td>add to 1 node only</td><td>4659.52</td><td>2253.78</td>
</tr>
<tr>
	<td>query</td><td>2799.79</td><td>2204.23</td>
</tr>
</table>