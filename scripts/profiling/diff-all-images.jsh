// Compares <campaign>-all-images.csv with the per-year CSVs: how many all-images rows
// equal a per-year row, differ from one (and in which fields), or are in no per-year CSV.
// The all-images CSV now stores only the last kind, so a healthy cache shows
// "equal to per-year 0, differ 0".
//
// From the repo root, after `sbt scalawiki-wlx/assembly`:
//   jshell -R-Xmx6g -R-Dcampaign=wlm-UA -R-Dfrom=2012 -R-Dto=2025 \
//     --class-path scalawiki-wlx/target/scala-2.13/scalawiki-wlx-<version>.jar \
//     scripts/profiling/diff-all-images.jsh
// Optional: -R-DcsvDir=<dir> (default csv-cache).
import org.scalawiki.dto.Image;

var imp = org.scalawiki.wlx.ImageCsvImporter$.MODULE$;
var dir = System.getProperty("csvDir", "csv-cache") + "/";
var campaign = System.getProperty("campaign", "wlm-UA");
int from = Integer.getInteger("from", 2012), to = Integer.getInteger("to", 2025);

var byId = new java.util.HashMap<Long, Image>();
int perYearRows = 0, perYearNoId = 0, dupAcrossYears = 0;
for (int y = from; y <= to; y++) {
  var it = imp.imagesFromCsv(dir + campaign + "-" + y + "-images.csv", imp.imagesFromCsv$default$2(), imp.imagesFromCsv$default$3(), false).iterator();
  while (it.hasNext()) {
    Image i = (Image) it.next();
    perYearRows++;
    if (i.pageId().isEmpty()) { perYearNoId++; continue; }
    if (byId.put((Long) i.pageId().get(), i) != null) dupAcrossYears++;
  }
}
System.out.println("per-year rows " + perYearRows + ", without page id " + perYearNoId + ", same page id in 2+ years " + dupAcrossYears);

int total = 0, equal = 0, extra = 0, extraWiki = 0, noId = 0, differ = 0;
var fieldDiffs = new java.util.TreeMap<String, Integer>();
var combos = new java.util.TreeMap<String, Integer>();
var examples = new java.util.ArrayList<String>();
var it2 = imp.imagesFromCsv(dir + campaign + "-all-images.csv", imp.imagesFromCsv$default$2(), imp.imagesFromCsv$default$3(), false).iterator();
while (it2.hasNext()) {
  Image a = (Image) it2.next();
  total++;
  if (a.pageId().isEmpty()) { noId++; continue; }
  Image p = byId.get((Long) a.pageId().get());
  if (p == null) {
    extra++;
    if (a.pageUrl().isDefined() && !((String) a.pageUrl().get()).contains("commons.wikimedia.org")) extraWiki++;
    continue;
  }
  if (p.equals(a)) { equal++; continue; }
  differ++;
  var names = new java.util.ArrayList<String>();
  var detail = new StringBuilder(a.title());
  for (int k = 0; k < a.productArity(); k++) {
    Object av = a.productElement(k), pv = p.productElement(k);
    if (!java.util.Objects.equals(av, pv)) {
      String n = a.productElementName(k);
      names.add(n);
      fieldDiffs.merge(n, 1, Integer::sum);
      String as = String.valueOf(av), ps = String.valueOf(pv);
      detail.append("\n    ").append(n).append(": all=").append(as.length() > 110 ? as.substring(0, 110) + "..." : as)
            .append(" | year=").append(ps.length() > 110 ? ps.substring(0, 110) + "..." : ps);
    }
  }
  combos.merge(String.valueOf(names), 1, Integer::sum);
  if (examples.size() < 6) examples.add(detail.toString());
}
System.out.println("all-images rows " + total + ": equal to per-year " + equal + ", differ " + differ + ", not in per-year " + extra + " (of which non-Commons " + extraWiki + "), no page id " + noId);
System.out.println("differing fields (row counts): " + fieldDiffs);
System.out.println("field combinations: " + combos);
examples.forEach(e -> System.out.println("  " + e));
/exit
