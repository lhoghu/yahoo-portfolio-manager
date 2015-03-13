select * from (
    select 
    100.0 * current_value / agg.value,
    price,
    dividends,
    symbol,
    cost,
    current_value,
    change,
    pct_change,
    total_change,
    total_pct_change
    from daily_pnl
    join (select sum(current_value) as value
        from daily_pnl) agg

    union select
    0.0,
    0.0,
    sum(dividends),
    'TOTAL',
    sum(cost),
    sum(current_value),
    sum(change),
    sum(change) / sum(current_value),
    sum(total_change),
    sum(current_value - cost +dividends)/sum(cost)
    from daily_pnl
) as t
order by case t.symbol when 'TOTAL' then 100 else 1 end
