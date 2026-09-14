// Estimates how much memory each text field of the cached per-year images takes,
// and how much it would take if equal values were shared.
//
// From the repo root, after `sbt scalawiki-wlx/assembly`:
//   jshell -R-Xmx6g -R-Dcampaign=wlm-UA -R-Dfrom=2012 -R-Dto=2025 \
//     --class-path scalawiki-wlx/target/scala-2.13/scalawiki-wlx-<version>.jar \
//     scripts/profiling/string-tally.jsh
// Optional: -R-DcsvDir=<dir> (default csv-cache).
import org.scalawiki.dto.Image;

var imp = org.scalawiki.wlx.ImageCsvImporter$.MODULE$;
var dir = System.getProperty("csvDir", "csv-cache") + "/";
var campaign = System.getProperty("campaign", "wlm-UA");
int from = Integer.getInteger("from", 2012), to = Integer.getInteger("to", 2025);

String[] names = {"title", "url", "pageUrl", "author", "mime", "camera", "exifDate", "category", "specialNomination", "monumentId"};
long[] cnt = new long[names.length], bytes = new long[names.length], distinctBytes = new long[names.length];
var ds = new java.util.ArrayList<java.util.HashSet<String>>();
for (int k = 0; k < names.length; k++) ds.add(new java.util.HashSet<>());

// String object (24 B) + byte[] (16 B header, 1 B/char if Latin-1 else 2 B/char), 8-byte aligned
long strBytes(String s) { boolean latin1 = true; for (int c = 0; c < s.length(); c++) if (s.charAt(c) > 255) { latin1 = false; break; } long arr = 16 + (latin1 ? s.length() : 2L * s.length()); return 24 + ((arr + 7) / 8) * 8; }
void add(int k, String s) { if (s == null || s.isEmpty()) return; long b = strBytes(s); cnt[k]++; bytes[k] += b; if (ds.get(k).add(s)) distinctBytes[k] += b; }
void addOpt(int k, scala.Option<?> o) { if (o.isDefined()) add(k, String.valueOf(o.get())); }

long images = 0;
for (int y = from; y <= to; y++) {
  var it = imp.imagesFromCsv(dir + campaign + "-" + y + "-images.csv", imp.imagesFromCsv$default$2()).iterator();
  while (it.hasNext()) {
    Image a = (Image) it.next(); images++;
    add(0, a.title()); addOpt(1, a.url()); addOpt(2, a.pageUrl()); addOpt(3, a.author()); addOpt(4, a.mime());
    if (a.metadata().isDefined()) { var m = (org.scalawiki.dto.ImageMetadata) a.metadata().get(); addOpt(5, m.camera()); addOpt(6, m.data().get("DateTimeOriginal")); }
    var ci = a.categories().iterator(); while (ci.hasNext()) add(7, (String) ci.next());
    var si = a.specialNominations().iterator(); while (si.hasNext()) add(8, (String) si.next());
    var mi = a.monumentIds().iterator(); while (mi.hasNext()) add(9, (String) mi.next());
  }
}
System.out.println("images: " + images);
System.out.println(String.format("%-18s %9s %9s %10s %11s", "field", "values", "est MB", "distinct", "distinct MB"));
long tb = 0, td = 0;
for (int k = 0; k < names.length; k++) { tb += bytes[k]; td += distinctBytes[k];
  System.out.println(String.format("%-18s %9d %9.1f %10d %11.1f", names[k], cnt[k], bytes[k] / 1048576.0, ds.get(k).size(), distinctBytes[k] / 1048576.0)); }
System.out.println(String.format("%-18s %9s %9.1f %10s %11.1f", "total", "", tb / 1048576.0, "", td / 1048576.0));
/exit
