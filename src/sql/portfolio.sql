select * from 
(
    select
        y.symbol as symbol, 
        p.currency,
        p.position, 
        p.strike, 
        y.price * f.fx, 
        y.change * f.fx, 
        y.change / y.price, 
        y.volume, 
        f.fx,
        f.prtfFx,
        (y.price * f.fx - p.strike) * p.position,
        (y.price * f.fx - p.strike) / p.strike

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
        "GBP",
        0.0,
        sum(p.position * p.strike * f.prtffx),
        sum(p.position * y.price * f.fx * f.prtffx),
        sum(p.position * y.change * f.fx * f.prtffx),
        sum(p.position * y.change * f.fx * f.prtffx) / sum(p.position * y.price * f.fx * f.prtffx),
        0.0,
        0.0,
        1,
        sum((y.price * f.fx - p.strike) * p.position * f.prtffx),
        sum((y.price * f.fx - p.strike) * p.position * f.prtffx) / sum(p.position * p.strike * f.prtffx)
        
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
