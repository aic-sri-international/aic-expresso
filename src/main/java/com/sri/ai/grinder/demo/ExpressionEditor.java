package com.sri.ai.grinder.demo;

import javax.swing.JPanel;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Font;

import javax.swing.JScrollPane;
import javax.swing.JTextPane;
import javax.swing.text.AbstractDocument;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.DocumentFilter;
import javax.swing.text.Style;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyleContext;
import javax.swing.text.StyledDocument;

public class ExpressionEditor extends JPanel {
	private static final long serialVersionUID = 1L;
	
	//
	private JTextPane textPane;
	
	
	public ExpressionEditor() {
		setLayout(new BorderLayout(0, 0));
		
		JScrollPane editorScrollPane = new JScrollPane();
		add(editorScrollPane, BorderLayout.CENTER);
		
		textPane = new JTextPane();
		editorScrollPane.setViewportView(textPane);
		
		postGUISetup();
	}
	
	public String getText() {
		String result = "";
		StyledDocument styledDoc = textPane.getStyledDocument();
		
		try {
			result = styledDoc.getText(0, styledDoc.getLength());
		} catch (BadLocationException ble) {
			ble.printStackTrace();
		}
		
		return result;
	}
	
	public void setText(String text) {
		try {
			StyledDocument styledDoc = textPane.getStyledDocument();
			styledDoc.remove(0, styledDoc.getLength());
			styledDoc.insertString(0, text, null);
		} catch (BadLocationException ble) {
			ble.printStackTrace();
		}
	}
	
	//
	// PRIVATE
	//
	private void postGUISetup() {
		StyledDocument styledDoc = textPane.getStyledDocument();
		if (styledDoc instanceof AbstractDocument) {
			AbstractDocument doc = (AbstractDocument)styledDoc;
		    doc.setDocumentFilter(new ExpressionFormatFilter());
		} 
		addStylesToDocument(styledDoc);
	}
	
	private void addStylesToDocument(StyledDocument doc) {
        //Initialize some styles.
        Style def = StyleContext.getDefaultStyleContext().
                        getStyle(StyleContext.DEFAULT_STYLE);
 
        Style regular = doc.addStyle("regular", def);
        StyleConstants.setFontFamily(def, Font.MONOSPACED);
 
        Style s = doc.addStyle("italic", regular);
        StyleConstants.setItalic(s, true);
 
        s = doc.addStyle("bold", regular);
        StyleConstants.setBold(s, true);
        StyleConstants.setForeground(s, Color.BLUE);
 
        s = doc.addStyle("small", regular);
        StyleConstants.setFontSize(s, 10);
 
        s = doc.addStyle("large", regular);
        StyleConstants.setFontSize(s, 16);
	}
	
	private class ExpressionFormatFilter extends DocumentFilter {
		public ExpressionFormatFilter() {
			
		}
		
		@Override
		public void remove(DocumentFilter.FilterBypass fb, int offset, int length) throws BadLocationException {
			super.remove(fb, offset, length);
			
			format((StyledDocument)fb.getDocument());
		}
		
		@Override
		public void insertString(DocumentFilter.FilterBypass fb, int offset, String string, AttributeSet attr) throws BadLocationException {
			super.insertString(fb, offset, string, attr);
			
			format((StyledDocument)fb.getDocument());
		}
		
		@Override
		public void replace(DocumentFilter.FilterBypass fb, int offset, int length, String text, AttributeSet attrs) throws BadLocationException {
			super.replace(fb, offset, length, text, attrs);
			
			format((StyledDocument)fb.getDocument());
		}
		
		private void format(StyledDocument styledDocument) throws BadLocationException {
			
			String expressionText = styledDocument.getText(0, styledDocument.getLength());
			for (int i = 0; i < expressionText.length(); i++) {
				if (i % 2 == 0) {
					styledDocument.setCharacterAttributes(i, 1, styledDocument.getStyle("bold"), true);
				} 
				else {
					styledDocument.setCharacterAttributes(i, 1, styledDocument.getStyle("regular"), true);
				}
			}
		}
	}
}
