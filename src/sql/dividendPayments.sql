select 
        d.symbol as symbol, 
        d.date as date, 
        d.dividend as dividend, 
        sum(p.position) as units, 
        d.dividend * sum(p.position) as payment 
from 
        dividendsTable as d, 
        portfolio as p 
where 
        d.symbol = p.symbol and 
        date(d.date) >= date(p.date) 
group by d.symbol, d.date 
order by d.date
