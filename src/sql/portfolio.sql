select * from 
(
    select
        y.symbol as symbol, 
        p.currency as currency,
        p.position as position, 
        p.strike as strike, 
        y.price * f.fx * case 
                when abs(y.price * f.fx - p.strike) / p.strike > 0.8
                        then 100.0
                when abs(y.price * f.fx - p.strike) / y.price > 0.8
                        then 0.01
                else 1.0 end as price, 
        y.change * f.fx * case 
                when abs(y.price * f.fx - p.strike) / p.strike > 0.8
                        then 100.0
                when abs(y.price * f.fx - p.strike) / y.price > 0.8
                        then 0.01
                else 1.0 end as change, 
        y.change / y.price as pct_change, 
        y.volume as volume, 
        f.fx as fx,
        f.prtfFx as prtf_fx,
        (y.price * f.fx * case 
                when abs(y.price * f.fx - p.strike) / p.strike > 0.8
                        then 100.0
                when abs(y.price * f.fx - p.strike) / y.price > 0.8
                        then 0.01
                else 1.0 end - p.strike) * p.position as total_change,
        (y.price * f.fx * case 
                when abs(y.price * f.fx - p.strike) / p.strike > 0.8
                        then 100.0
                when abs(y.price * f.fx - p.strike) / y.price > 0.8
                        then 0.01
                else 1.0 end - p.strike) / p.strike as total_pct_change

        from 
            yahooQuotesTable as y, 
            portfolio as p, 
            fx as f

        where 
            p.symbol = y.symbol and
            f.toCcy = p.currency and
            f.fromCcy = y.currency
     
    union select
        "TOTAL" as symbol,
        "GBP" as currency,
        0.0 as position,
        sum(p.position * p.strike * f.prtffx) as strike,
        sum(p.position * y.price * f.fx * f.prtffx * case 
                when abs(y.price * f.fx - p.strike) / p.strike > 0.8
                        then 100.0
                when abs(y.price * f.fx - p.strike) / y.price > 0.8
                        then 0.01
                else 1.0 end) as price,
        sum(p.position * y.change * f.fx * f.prtffx * case 
                when abs(y.price * f.fx - p.strike) / p.strike > 0.8
                        then 100.0
                when abs(y.price * f.fx - p.strike) / y.price > 0.8
                        then 0.01
                else 1.0 end) as change,
        sum(p.position * y.change * f.fx * f.prtffx * case 
                when abs(y.price * f.fx - p.strike) / p.strike > 0.8
                        then 100.0
                when abs(y.price * f.fx - p.strike) / y.price > 0.8
                        then 0.01
                else 1.0 end) / sum(p.position * y.price * f.fx * f.prtffx) as pct_change,
        0.0 as volume,
        1.0 as fx,
        1 as prtf_fx,
        sum((y.price * f.fx * case 
                when abs(y.price * f.fx - p.strike) / p.strike > 0.8
                        then 100.0
                when abs(y.price * f.fx - p.strike) / y.price > 0.8
                        then 0.01
                else 1.0 end - p.strike) * p.position * f.prtffx) as total_change,
        sum((y.price * f.fx * case 
                when abs(y.price * f.fx - p.strike) / p.strike > 0.8
                        then 100.0
                when abs(y.price * f.fx - p.strike) / y.price > 0.8
                        then 0.01
                else 1.0 end - p.strike) * p.position * f.prtffx) / sum(p.position * p.strike * f.prtffx) as total_pct_change

        from 
            yahooQuotesTable as y,
            portfolio as p,
            fx as f

        where 
            p.symbol = y.symbol and
            f.toCcy = p.currency and
            f.fromCcy = y.currency
        )
order by case symbol when "TOTAL" then 100 else 1 end
