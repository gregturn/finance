package com.greglturnquist.finance;

import java.util.List;
import java.util.stream.IntStream;

import org.springframework.boot.autoconfigure.SpringBootApplication;

@SpringBootApplication
public class FinanceApplication {

	record Performance(int year, double performance) {
	}

	static final List<Performance> snp = List.of( //
			new Performance(1951, 16.3), //
			new Performance(1952, 11.8), //
			new Performance(1953, -6.6), //
			new Performance(1954, 26.4), //
			new Performance(1955, 26.4), //
			new Performance(1956, 2.6), //
			new Performance(1957, -14.3), //
			new Performance(1958, 38.1), //
			new Performance(1959, 8.5), //
			new Performance(1960, -3.0), //
			new Performance(1961, 23.1), //
			new Performance(1962, -11.8), //
			new Performance(1963, 18.9), //
			new Performance(1964, 13.0), //
			new Performance(1965, 9.1), //
			new Performance(1966, -13.1), //
			new Performance(1967, 20.1), //
			new Performance(1968, 7.7), //
			new Performance(1969, -11.4), //
			new Performance(1970, 0.1), //
			new Performance(1971, 10.8), //
			new Performance(1972, 15.6), //
			new Performance(1973, -17.4), //
			new Performance(1974, -29.7), //
			new Performance(1975, 31.5), //
			new Performance(1976, 19.1), //
			new Performance(1977, -11.5), //
			new Performance(1978, 1.1), //
			new Performance(1979, 12.3), //
			new Performance(1980, 25.8), //
			new Performance(1981, -9.7), //
			new Performance(1982, 14.8), //
			new Performance(1983, 17.3), //
			new Performance(1984, 1.4), //
			new Performance(1985, 26.3), //
			new Performance(1986, 14.6), //
			new Performance(1987, 2.0), //
			new Performance(1988, 12.4), //
			new Performance(1989, 27.3), //
			new Performance(1990, -6.6), //
			new Performance(1991, 26.3), //
			new Performance(1992, 4.5), //
			new Performance(1993, 7.1), //
			new Performance(1994, -1.5), //
			new Performance(1995, 34.1), //
			new Performance(1996, 20.3), //
			new Performance(1997, 31.0), //
			new Performance(1998, 26.7), //
			new Performance(1999, 19.5), //
			new Performance(2000, -10.1), //
			new Performance(2001, -13.0), //
			new Performance(2002, -23.4), //
			new Performance(2003, 26.4), //
			new Performance(2004, 9.0), //
			new Performance(2005, 3.0), //
			new Performance(2006, 13.6), //
			new Performance(2007, 3.5), //
			new Performance(2008, -38.5), //
			new Performance(2009, 23.5), //
			new Performance(2010, 12.8), //
			new Performance(2011, 0.0), //
			new Performance(2012, 13.41), //
			new Performance(2013, 29.6), //
			new Performance(2014, 11.39), //
			new Performance(2015, -0.73), //
			new Performance(2016, 9.54), //
			new Performance(2017, 19.42), //
			new Performance(2018, -6.24), //
			new Performance(2019, 28.88), //
			new Performance(2020, 16.26), //
			new Performance(2021, 26.89), //
			new Performance(2022, -19.44));

	double round(double x, int digits) {

		var factor = Math.pow(10, digits);
		return Math.round(x * factor) / factor;
	}

	static double aMean(List<Performance> xs) {

		return xs.stream() //
				.reduce(0.0, (subtotal, element) -> subtotal + element.performance, Double::sum) / xs.size();
	}

	static double absChange(double relChange) {
		return 1.0 + relChange / 100.0;
	}

	static double relChange(double absChange) {
		return 100.0 * (absChange - 1.0);
	}

	static double actualAbsGrowth(List<Performance> xs) {

		return xs.stream() //
				.reduce(1.0, (subtotal, element) -> subtotal * absChange(element.performance), Double::sum);
	}

	static double gMean(List<Performance> xs) {

		double nthRoot = Math.pow(actualAbsGrowth(xs), 1.0 / xs.size());
		return relChange(nthRoot);
	}

	static double stddev(List<Double> xs) {

		var mean = xs.stream().reduce(0.0, Double::sum) / xs.size();
		var squareSum = xs.stream().reduce(0.0, (subtotal, item) -> subtotal + Math.pow(item - mean, 2));
		return Math.sqrt(squareSum / xs.size());
	}

	record IulCaps(double lower, double upper) {
		double apply(double x) {
			return List.of(x, lower, upper).stream().sorted().toList().get(1);
		}
	}

	static List<Performance> iul(List<Performance> xs, IulCaps caps) {

		return xs.stream() //
				.map(performance -> new Performance(performance.year, caps.apply(performance.performance))) //
				.toList();
	}

	record Series(int firstYear, int lastYear, double aMean, double gMean, List<Performance> sublist) {
	}

	static List<Series> series(List<Performance> xs, int years) {

		if (years > xs.size()) {
			return List.of();
		}

		return IntStream.range(0, xs.size() - years + 1) //
				.mapToObj(start -> xs.subList(start, start + years)) //
				.map(sublist -> new Series(sublist.get(0).year(), sublist.get(sublist.size() - 1).year(), aMean(sublist),
						gMean(sublist), sublist)) //
				.toList();
	}

	record StatsData(double avgGeomMean, double minGeomMean, double maxGeomMean, double stddev, Series minList,
			Series maxList) {
	}

	static StatsData calc_stats(List<Series> xs) {

		var aMeans = xs.stream().map(series -> series.aMean).toList();
		var gMeans = xs.stream().map(series -> series.gMean).toList();

		var minList = tail(xs).stream().reduce(head(xs),
				(min, item) -> gMean(item.sublist) < gMean(min.sublist) ? item : min);

		var maxList = tail(xs).stream().reduce(head(xs),
				(max, item) -> gMean(item.sublist) > gMean(max.sublist) ? item : max);

		return new StatsData( //
				gMeans.stream().reduce(0.0, Double::sum) / xs.size(), //
				gMeans.stream().reduce(Double.MAX_VALUE, Double::min), //
				gMeans.stream().reduce(Double.MIN_VALUE, Double::max), //
				stddev(gMeans), //
				minList, //
				maxList //
		);
	}

	static <T> T head(List<T> xs) {
		return xs.get(0);
	}

	static <T> List<T> tail(List<T> xs) {
		return xs.subList(1, xs.size() - 1);
	}

	public static void main(String[] args) {
		// SpringApplication.run(FinanceApplication.class, args);

		System.out.println("S&P 500 performance = " + snp);
		System.out.println(String.format("S&P 500 Arithmetic mean (%d to %d) = %.1f%%", snp.get(0).year,
				snp.get(snp.size() - 1).year, aMean(snp)));
		System.out.println(String.format("S&P 500 Geometric mean (CAGR) (%d to %d) = %.1f%%", snp.get(0).year,
				snp.get(snp.size() - 1).year, +gMean(snp)));
		System.out.println(String.format("S&P 500 Actual total growth factor = %.1fx", actualAbsGrowth(snp)));
		System.out.println();

		var iulData = iul(snp, new IulCaps(0.0, 16.0));

		System.out.println("IUL performance = " + iulData);
		System.out.println(String.format("IUL arithmetic performance (%d to %d) = %.1f%%", iulData.get(0).year,
				iulData.get(iulData.size() - 1).year, aMean(iulData)));
		System.out.println(String.format("IUL geometric performance (%d to %d) = %.1f%%", iulData.get(0).year,
				iulData.get(iulData.size() - 1).year, gMean(iulData)));
		System.out.println(String.format("Actual IUL total growth factor (%d to %d) = %.1fx", iulData.get(0).year,
				iulData.get(iulData.size() - 1).year, actualAbsGrowth(iulData)));
		System.out.println();

		if (actualAbsGrowth(iulData) > actualAbsGrowth(snp)) {
			System.out.println(String.format("A geometric growth difference of %.1f%% (%.1f%% over %.1f%%) means that...",
					(gMean(iulData) - gMean(snp)), gMean(iulData), gMean(snp)));
			System.out.println(
					String.format("IUL beats S&P 500 by a factor of: %.1fx", actualAbsGrowth(iulData) / actualAbsGrowth(snp)));
		} else {
			System.out.println(String.format("A geometric growth difference of %.1f%% (%.1f%% over %.1f%%) means that...",
					(gMean(snp) - gMean(iulData)), gMean(snp), gMean(iulData)));
			System.out.println(
					String.format("S&P 500 beats IUL by a factor of: %.1fx", actualAbsGrowth(snp) / actualAbsGrowth(iulData)));
		}
		System.out.println();

		System.out.println("""
				This is where every 10, 15, 20, etc. year interval in all the data is evaluated and then averaged together.
				The min and max performance of each interval is displayed, and the 1st stddev is shown. There is a 68%
				that actual performance is within 1 stddev of the average.
				""");

		List.of(10, 15, 20, 25, 30).forEach(interval -> {

			var snp500Stats = calc_stats(series(snp, interval));
			var iulStats = calc_stats(series(iulData, interval));

			System.out.println(interval + "-year stats");
			System.out.println("=========================");

			List.of(new DataSeries("S&P 500", snp500Stats), new DataSeries("IUL", iulStats)).forEach(dataSeries -> {

				System.out
						.println(String.format("%s stats:\t Avg geom mean = %.1f%% (%.1f%%..%.1f%%)\t68%% chance between %.1f%% and %.1f%%", //
								dataSeries.desc, //
								dataSeries.stats.avgGeomMean, //
								dataSeries.stats.minGeomMean, //
								dataSeries.stats.maxGeomMean, //
								dataSeries.stats.avgGeomMean - dataSeries.stats.stddev, //
								dataSeries.stats.avgGeomMean + dataSeries.stats.stddev));
				System.out.println(
						"Windows displayed are (first year, last year, arithmetic mean, geom mean, and the original data)");
				System.out.println("Min window is " + dataSeries.stats.minList);
				System.out.println("Max window is " + dataSeries.stats.maxList);

				System.out.println();
			});

		});
	}

	record DataSeries(String desc, StatsData stats) {
	}

}
