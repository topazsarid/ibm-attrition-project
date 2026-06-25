const pptxgen = require("pptxgenjs");
const p = new pptxgen();
p.defineLayout({ name: "W", width: 13.333, height: 7.5 });
p.layout = "W";

// ---- palette ----
const NAVY = "1F4E5F", DARK = "16384A", ICE = "DCE6E8", ICELT = "EAF1F3",
      ACCENT = "C0622E", TEXT = "1A1A1A", MUTED = "5B6B70", WHITE = "FFFFFF";
const HF = "Cambria", BF = "Calibri";
const W = 13.333;

// ---- helpers ----
function titleBar(s, t) {
  s.addText(t, { x: 0.6, y: 0.42, w: W - 1.2, h: 0.85, fontFace: HF, fontSize: 30,
    bold: true, color: NAVY, align: "left", valign: "middle" });
}
function note(s, txt) { s.addNotes(txt); }

// ====== SLIDE 1 — TITLE ======
let s = p.addSlide(); s.background = { color: DARK };
s.addText("THE CUMULATIVE ATTRITION RISK HYPOTHESIS", { x: 0.9, y: 1.7, w: 11.5, h: 1.3,
  fontFace: HF, fontSize: 38, bold: true, color: WHITE, align: "left", lineSpacingMultiple: 1.0 });
s.addText("Strain, Embeddedness, and the Limits of Pay", { x: 0.9, y: 3.0, w: 11.5, h: 0.6,
  fontFace: BF, fontSize: 22, color: ICE, align: "left" });
s.addText("“Employees rarely leave because of one factor — work-life strain accumulates, and pay only helps up to a point.”",
  { x: 0.9, y: 3.85, w: 10.6, h: 0.9, fontFace: BF, fontSize: 16, italic: true, color: "AEC6CF", align: "left" });
s.addText("Tzlil Hayne  ·  Oded Shmuely  ·  Amit Shaimen  ·  Topaz Sarid",
  { x: 0.9, y: 6.2, w: 11.5, h: 0.4, fontFace: BF, fontSize: 15, color: WHITE, align: "left" });
s.addText("SISE2601  ·  Data Analysis Project", { x: 0.9, y: 6.62, w: 11.5, h: 0.35,
  fontFace: BF, fontSize: 12, color: "8FB0BB", align: "left" });
note(s, "One-line hook: people don't leave over one thing; strain stacks up, and pay can't fully fix it. State the title and the team. Keep it to ~20 seconds.");

// ====== SLIDE 2 — WHY IT MATTERS ======
s = p.addSlide(); s.background = { color: WHITE };
titleBar(s, "Why employee attrition matters");
s.addText("Replacing an employee is costly — and in this dataset, 16.1% of employees left.",
  { x: 0.6, y: 1.35, w: 12.1, h: 0.5, fontFace: BF, fontSize: 17, color: MUTED });
const boxes = ["An employee\nleaves", "Recruiting &\ntraining a replacement", "Lost productivity\nuntil ramp-up"];
const bw = 3.2, bh = 1.7, gap = 0.95, startX = (W - (bw * 3 + gap * 2)) / 2, by = 3.1;
boxes.forEach((b, i) => {
  const x = startX + i * (bw + gap);
  s.addShape(p.ShapeType.roundRect, { x, y: by, w: bw, h: bh, fill: { color: ICELT },
    line: { color: NAVY, width: 1 }, rectRadius: 0.1 });
  s.addText(b, { x, y: by, w: bw, h: bh, fontFace: BF, fontSize: 16, bold: true, color: NAVY,
    align: "center", valign: "middle" });
  if (i < 2) s.addText("→", { x: x + bw, y: by, w: gap, h: bh, fontFace: BF, fontSize: 30,
    color: ACCENT, align: "center", valign: "middle" });
});
s.addText("Understanding why people leave helps focus retention where it matters.",
  { x: 0.6, y: 5.5, w: 12.1, h: 0.5, fontFace: BF, fontSize: 15, italic: true, color: MUTED, align: "center" });
note(s, "Attrition is expensive: recruiting, training, and lost productivity until the new hire ramps up. So firms want to know WHY people leave. In our data, 16.1% left. ~40 sec.");

// ====== SLIDE 3 — PRIOR WORK ======
s = p.addSlide(); s.background = { color: WHITE };
titleBar(s, "What we build on");
const hdr = (t) => ({ text: t, options: { fill: NAVY, color: WHITE, bold: true, fontSize: 16, fontFace: BF, align: "left", valign: "middle" } });
const cell = (t, c) => ({ text: t, options: { color: c || TEXT, fontSize: 14, fontFace: BF, align: "left", valign: "middle", fill: WHITE } });
const rows = [
  [hdr("Prior research"), hdr("Our extension")],
  [cell("Pay predicts turnover, but is not the strongest factor (Griffeth et al., 2000)"), cell("We test whether pay still helps under high strain", NAVY)],
  [cell("Overtime and heavy demands raise work pressure and turnover (Wong et al., 2024)"), cell("We test the accumulation of factors, not each one alone", NAVY)],
  [cell("Embeddedness: workers with more ties leave less (Mitchell et al., 2001)"), cell("We use single status as a marker of lower embeddedness", NAVY)],
];
s.addTable(rows, { x: 0.6, y: 1.5, w: 12.1, colW: [6.05, 6.05], rowH: [0.55, 1.1, 1.1, 1.1],
  border: { type: "solid", color: "C9D6DB", pt: 1 }, margin: [6, 8, 6, 8], valign: "middle" });
note(s, "Show we know the field. Three strands: (1) pay matters but is secondary; (2) work demands drive turnover; (3) embeddedness explains why singles leave more. Our contribution is the right column — we test accumulation and the limits of pay. ~70 sec. This slide answers the 20-point 'prior work' criterion.");

// ====== SLIDE 4 — RESEARCH QUESTION ======
s = p.addSlide(); s.background = { color: DARK };
s.addText("RESEARCH QUESTION", { x: 0.9, y: 1.4, w: 11, h: 0.5, fontFace: BF, fontSize: 18,
  bold: true, color: ACCENT, charSpacing: 2 });
s.addText("How does the accumulation of overtime, business travel, and single status relate to employee attrition — and does higher pay weaken this link?",
  { x: 0.9, y: 2.1, w: 11.3, h: 2.0, fontFace: HF, fontSize: 30, bold: true, color: WHITE, lineSpacingMultiple: 1.05 });
s.addShape(p.ShapeType.roundRect, { x: 0.9, y: 4.7, w: 11.3, h: 1.3, fill: { color: "1F4E5F" }, line: { color: ACCENT, width: 1.5 }, rectRadius: 0.08 });
s.addText([{ text: "Hypothesis:  ", options: { bold: true, color: ACCENT } },
  { text: "attrition increases as strain markers accumulate, while pay becomes less protective under high strain.", options: { color: WHITE } }],
  { x: 1.15, y: 4.7, w: 10.8, h: 1.3, fontFace: BF, fontSize: 17, valign: "middle" });
note(s, "Read the question once, clearly. Then the hypothesis: more strain markers → more leaving, and pay helps less as strain rises. ~30 sec.");

// ====== SLIDE 5 — DATA OVERVIEW ======
s = p.addSlide(); s.background = { color: WHITE };
titleBar(s, "The data");
const dcell = (t, b) => ({ text: t, options: { color: b ? NAVY : TEXT, bold: !!b, fontSize: 14, fontFace: BF, align: "left", valign: "middle", fill: WHITE } });
const drows = [
  [dcell("Dataset", true), dcell("IBM HR Analytics Employee Attrition (synthetic)")],
  [dcell("Unit of analysis", true), dcell("One employee per row")],
  [dcell("Employees", true), dcell("1,470  (no missing values)")],
  [dcell("Target", true), dcell("Attrition — Yes / No")],
  [dcell("Key features", true), dcell("Overtime, business travel, marital status, income, job level, age, tenure")],
];
s.addTable(drows, { x: 0.6, y: 1.55, w: 8.0, colW: [2.5, 5.5], rowH: 0.72,
  border: { type: "solid", color: "C9D6DB", pt: 1 }, margin: [5, 8, 5, 8], valign: "middle" });
// stat callout
s.addShape(p.ShapeType.roundRect, { x: 9.0, y: 1.7, w: 3.6, h: 3.4, fill: { color: NAVY }, line: { type: "none" }, rectRadius: 0.1 });
s.addText("16.1%", { x: 9.0, y: 2.1, w: 3.6, h: 1.3, fontFace: HF, fontSize: 60, bold: true, color: WHITE, align: "center" });
s.addText("of employees left", { x: 9.0, y: 3.4, w: 3.6, h: 0.5, fontFace: BF, fontSize: 17, color: ICE, align: "center" });
s.addText("237 of 1,470  ·  imbalanced classes", { x: 9.0, y: 3.95, w: 3.6, h: 0.5, fontFace: BF, fontSize: 13, color: "AEC6CF", align: "center" });
note(s, "Quick data tour: 1,470 employees, synthetic IBM dataset, no missing values. Target is leave/stay; only 16.1% left, so the classes are imbalanced. We verified there are no impossible age/experience values. ~40 sec.");

// ====== SLIDE 6 — METHOD ======
s = p.addSlide(); s.background = { color: WHITE };
titleBar(s, "How we tested it");
const steps = [
  ["1", "Data preparation", "Clean the dataset, drop non-informative columns, and select the key variables."],
  ["2", "Feature construction", "Build a strain score (0–3) from overtime + any travel + single, and split income into low / medium / high."],
  ["3", "Analysis", "Fit logistic regression, add seniority controls, and run validity checks."],
];
const cw = 3.78, cgap = 0.45, cstart = (W - (cw * 3 + cgap * 2)) / 2, cy = 1.7, ch = 3.7;
steps.forEach((st3, i) => {
  const x = cstart + i * (cw + cgap);
  s.addShape(p.ShapeType.roundRect, { x, y: cy, w: cw, h: ch, fill: { color: ICELT }, line: { color: "C9D6DB", width: 1 }, rectRadius: 0.08 });
  s.addShape(p.ShapeType.ellipse, { x: x + cw / 2 - 0.45, y: cy + 0.35, w: 0.9, h: 0.9, fill: { color: ACCENT }, line: { type: "none" } });
  s.addText(st3[0], { x: x + cw / 2 - 0.45, y: cy + 0.35, w: 0.9, h: 0.9, fontFace: HF, fontSize: 30, bold: true, color: WHITE, align: "center", valign: "middle" });
  s.addText(st3[1], { x: x + 0.2, y: cy + 1.45, w: cw - 0.4, h: 0.5, fontFace: BF, fontSize: 18, bold: true, color: NAVY, align: "center" });
  s.addText(st3[2], { x: x + 0.25, y: cy + 2.0, w: cw - 0.5, h: 1.5, fontFace: BF, fontSize: 14, color: TEXT, align: "center", valign: "top", lineSpacingMultiple: 1.05 });
});
note(s, "Three steps: prepare the data, build the strain score and pay groups, then run logistic regression with seniority controls and validity checks. Emphasize the strain score is just a 0–3 count. ~50 sec.");

// ====== SLIDE 7 — RESULT 1 ======
s = p.addSlide(); s.background = { color: WHITE };
titleBar(s, "Attrition rises sharply as strain factors accumulate");
s.addImage({ path: "dose_response.png", x: 4.7, y: 1.55, w: 8.1, h: 5.18 });
s.addText("50%", { x: 0.6, y: 2.2, w: 3.7, h: 1.1, fontFace: HF, fontSize: 64, bold: true, color: ACCENT, align: "center" });
s.addText("leave with all three markers", { x: 0.6, y: 3.3, w: 3.7, h: 0.5, fontFace: BF, fontSize: 16, bold: true, color: NAVY, align: "center" });
s.addText("vs. just 2.5% with none — each added marker roughly triples the odds of leaving.",
  { x: 0.6, y: 3.95, w: 3.8, h: 1.5, fontFace: BF, fontSize: 14, color: MUTED, align: "center", lineSpacingMultiple: 1.1 });
note(s, "Headline finding. As markers stack from 0 to 3, attrition climbs 2.5% → 8.3% → 20.3% → 50%. Each extra marker roughly triples the odds (OR ≈ 3.2), and the trend is significant. This is the core of the project. ~50 sec.");

// ====== SLIDE 8 — RESULT 2 ======
s = p.addSlide(); s.background = { color: WHITE };
titleBar(s, "Pay helps under low strain — but cannot offset high strain");
s.addImage({ path: "heatmap.png", x: 0.55, y: 1.6, w: 7.9, h: 4.9 });
s.addShape(p.ShapeType.roundRect, { x: 8.8, y: 2.0, w: 4.0, h: 3.6, fill: { color: ICELT }, line: { color: "C9D6DB", width: 1 }, rectRadius: 0.08 });
s.addText([
  { text: "At low strain, ", options: { bold: true, color: NAVY } },
  { text: "higher pay clearly lowers attrition.\n\n", options: { color: TEXT } },
  { text: "At high strain, ", options: { bold: true, color: ACCENT } },
  { text: "attrition stays high even for medium- and high-pay employees — extra pay no longer helps.", options: { color: TEXT } },
], { x: 9.05, y: 2.25, w: 3.5, h: 3.1, fontFace: BF, fontSize: 15, valign: "top", lineSpacingMultiple: 1.05 });
note(s, "Reading the heatmap: down each column, pay lowers attrition; but across the top row (high strain) it flattens. Single employees who work overtime stay around 36% even at high pay. Money buys back one factor, not several. Note small cells — read as indicative. ~50 sec.");

// ====== SLIDE 9 — VALIDITY / LIMITATIONS / CONCLUSION ======
s = p.addSlide(); s.background = { color: WHITE };
titleBar(s, "Validity, limitations & takeaway");
const blk = (x, head, body, hc) => {
  s.addText(head, { x, y: 1.6, w: 3.78, h: 0.5, fontFace: BF, fontSize: 18, bold: true, color: hc });
  s.addText(body, { x, y: 2.15, w: 3.78, h: 3.0, fontFace: BF, fontSize: 14, color: TEXT, valign: "top", lineSpacingMultiple: 1.1 });
};
blk(0.6, "Validity", "Controlled for age, job level, and tenure. Checked whether the income effect is really seniority — it largely is (a Simpson's-paradox pattern).", NAVY);
blk(4.78, "Limitations", "Dataset is synthetic. Cross-sectional — associations, not causation. The highest-strain pay groups are small, so exact percentages are indicative.", NAVY);
s.addShape(p.ShapeType.roundRect, { x: 8.85, y: 1.5, w: 3.9, h: 4.4, fill: { color: NAVY }, line: { type: "none" }, rectRadius: 0.08 });
s.addText("Takeaway", { x: 9.1, y: 1.75, w: 3.4, h: 0.5, fontFace: BF, fontSize: 18, bold: true, color: ACCENT });
s.addText("Strain accumulates, and pay is not a complete retention strategy.\n\nTo keep people, reduce stacked strain — don't rely on pay alone.",
  { x: 9.1, y: 2.35, w: 3.45, h: 3.3, fontFace: BF, fontSize: 16, color: WHITE, valign: "top", lineSpacingMultiple: 1.15 });
note(s, "Close strong. Validity: we ruled out seniority as the real driver of the pay effect. Limitations: synthetic, cross-sectional, small cells. Takeaway: strain stacks, and pay alone won't retain people. ~50 sec.");

// ====== SLIDE 10 — CLOSING ======
s = p.addSlide(); s.background = { color: DARK };
s.addText("Thank you", { x: 0.9, y: 2.4, w: 11.5, h: 1.0, fontFace: HF, fontSize: 44, bold: true, color: WHITE });
s.addText("Questions?", { x: 0.9, y: 3.5, w: 11.5, h: 0.7, fontFace: BF, fontSize: 24, color: ICE });
s.addText("The Cumulative Attrition Risk Hypothesis  ·  Tzlil, Oded, Amit, Topaz",
  { x: 0.9, y: 6.3, w: 11.5, h: 0.4, fontFace: BF, fontSize: 14, color: "8FB0BB" });
note(s, "Thank the audience and invite questions. Make sure to finish on time.");

p.writeFile({ fileName: "presentation.pptx" }).then(f => console.log("Saved:", f));
