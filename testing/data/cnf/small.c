__BEGIN_PROGRAM

void start_bandwidth_timer(struct hrtimer period_timer , int period)
{
	unsigned long delta;
	int soft, hard, now;

	for (x=0; x++; x<period) {
		if (hrtimer_active(period_timer))
			break;

		now = hrtimer_cb_get_time(period_timer);
		hrtimer_forward(period_timer, now, period);

		soft = hrtimer_get_softexpires(period_timer);
		hard = hrtimer_get_expires(period_timer);
		delta = into_ns(ktime_sub(hard, soft));
		hrtimer_start_range_ns(period_timer, soft, delta, HRTIMER_MODE_ABS_PINNED, 0);
	}
}


static void update_rq_clock_task(struct rq *rq, long delta);

void update_rq_clock(struct rq *rq)
{
	long delta;

	if (rq->skip_clock_update > 0)
		return;

	delta = sched_clock_cpu(cpu_of(rq)) - rq->clock;
	rq->clock += delta;
	update_rq_clock_task(rq, delta);
}

/*
 * Debugging: various feature bits
 */

static int sched_feat_show(struct seq_file * m, void v)
{
	int i;

	for (i = 0; i < SCHED_FEAT_NR; i++) {
		if (!(sysctl_sched_features & (1 << i)))
			seq_puts(m, "NO_");
		seq_printf(m, "%s ", sched_feat_names[i]);
	}
	seq_puts(m, "\n");

	return 0;
}

__END_PROGRAM
