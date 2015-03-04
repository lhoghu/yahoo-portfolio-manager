create view daily_pnl as select
        t.payments as dividends,
        t.symbol as symbol,
        sum(t.cost) as cost,
        sum(t.current_value * t.rescale) as current_value,
        sum(t.change * t.rescale) as change,
        t.pct_change as pct_change,
        sum(t.current_value * t.rescale - t.cost) + ifnull(t.payments, 0.0) as total_change,
        (sum(t.current_value * t.rescale - t.cost) + ifnull(t.payments, 0.0)) / sum(t.cost) as total_pct_change
from 
(
    select
        y.symbol as symbol, 
        y.price * f.fx as price,
        p.strike * p.position * f.prtfFx as cost, 
        y.price * f.fx * p.position as current_value,
        case 
                when abs(y.price * f.fx - p.strike) / p.strike > 0.8 
                        and abs(y.price * f.fx - p.strike) / p.strike < 1.0
                        then 100.0
                when abs(y.price * f.fx - p.strike) / y.price > 0.8
                        and abs(y.price * f.fx - p.strike) / y.price < 1.0
                        then 0.01
                else 1.0 end as rescale, 
        d.divs as payments,
        y.change * f.fx * p.position as change, 
        y.change / y.price as pct_change

        from 
            yahooQuotesTable as y, 
            portfolio as p, 
            fx as f

        left outer join 
        (
                select 
                        c.symbol as symbol,
                        sum(c.payment) as divs
                from
                (
                        select 
                                d.*, 
                                d.dividend * sum(p.position) as payment 
                        from 
                                dividendsTable as d,
                                portfolio as p 
                        where 
                                d.symbol = p.symbol and 
                                date(d.date) >= date(p.date) 
                        group by d.symbol, d.date
                ) as c 
                group by c.symbol
        ) as d
        on d.symbol = p.symbol

        where 
            p.symbol = y.symbol and
            f.toCcy = p.currency and
            f.fromCcy = y.currency

) as t
group by t.symbol;
