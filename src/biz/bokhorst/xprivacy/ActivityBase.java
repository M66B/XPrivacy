package biz.bokhorst.xprivacy;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.content.res.TypedArray;
import android.graphics.Bitmap;
import android.graphics.Canvas;
import android.graphics.Paint;
import android.graphics.Bitmap.Config;
import android.graphics.PorterDuff.Mode;
import android.graphics.drawable.Drawable;
import android.util.TypedValue;

@SuppressLint("Registered")
public class ActivityBase extends Activity {

	protected Bitmap[] getTriStateCheckBox() {
		Bitmap[] bitmap = new Bitmap[4];

		// Get highlight color
		TypedArray ta1 = getTheme().obtainStyledAttributes(new int[] { android.R.attr.colorActivatedHighlight });
		int highlightColor = ta1.getColor(0, 0xFF00FF);
		ta1.recycle();

		// Get off check box
		TypedArray ta2 = getTheme().obtainStyledAttributes(new int[] { android.R.attr.listChoiceIndicatorMultiple });
		Drawable off = ta2.getDrawable(0);
		ta2.recycle();
		off.setBounds(0, 0, off.getIntrinsicWidth(), off.getIntrinsicHeight());

		// Get check mark
		Drawable checkmark = getResources().getDrawable(R.drawable.checkmark);
		checkmark.setBounds(0, 0, off.getIntrinsicWidth(), off.getIntrinsicHeight());
		checkmark.setColorFilter(highlightColor, Mode.SRC_ATOP);

		// Get check mark outline
		Drawable checkmarkOutline = getResources().getDrawable(R.drawable.checkmark_outline);
		checkmarkOutline.setBounds(0, 0, off.getIntrinsicWidth(), off.getIntrinsicHeight());

		// Create off check box
		bitmap[0] = Bitmap.createBitmap(off.getIntrinsicWidth(), off.getIntrinsicHeight(), Config.ARGB_8888);
		Canvas canvas0 = new Canvas(bitmap[0]);
		off.draw(canvas0);

		// Create half check box
		bitmap[1] = Bitmap.createBitmap(off.getIntrinsicWidth(), off.getIntrinsicHeight(), Config.ARGB_8888);
		Canvas canvas1 = new Canvas(bitmap[1]);
		off.draw(canvas1);
		Paint paint1 = new Paint();
		paint1.setStyle(Paint.Style.FILL);
		paint1.setColor(highlightColor);
		float wborder = off.getIntrinsicWidth() / 3f;
		float hborder = off.getIntrinsicHeight() / 3f;
		canvas1.drawRect(wborder, hborder, off.getIntrinsicWidth() - wborder, off.getIntrinsicHeight() - hborder,
				paint1);

		// Create full check box
		bitmap[2] = Bitmap.createBitmap(off.getIntrinsicWidth(), off.getIntrinsicHeight(), Config.ARGB_8888);
		Canvas canvas2 = new Canvas(bitmap[2]);
		off.draw(canvas2);
		checkmark.draw(canvas2);
		checkmarkOutline.draw(canvas2);

		// Get question mark
		Drawable questionmark = getResources().getDrawable(R.drawable.ondemand);
		questionmark.setBounds(0, 0, off.getIntrinsicWidth(), off.getIntrinsicHeight());
		questionmark.setColorFilter(highlightColor, Mode.SRC_ATOP);

		// Get question mark outline
		Drawable questionmarkOutline = getResources().getDrawable(R.drawable.questionmark_outline);
		questionmarkOutline.setBounds(0, 0, off.getIntrinsicWidth(), off.getIntrinsicHeight());

		// Create question check box
		bitmap[3] = Bitmap.createBitmap(off.getIntrinsicWidth(), off.getIntrinsicHeight(), Config.ARGB_8888);
		Canvas canvas3 = new Canvas(bitmap[3]);
		off.draw(canvas3);
		questionmark.draw(canvas3);
		questionmarkOutline.draw(canvas3);

		return bitmap;
	}

	public int getThemed(int attr) {
		TypedValue tv = new TypedValue();
		getTheme().resolveAttribute(attr, tv, true);
		return tv.resourceId;
	}
}
