package com.sri.ai.grinder.ui;

import javax.swing.JPanel;
import java.awt.BorderLayout;
import javax.swing.JLabel;
import javax.swing.JTextField;
import javax.swing.JButton;

import java.awt.GridLayout;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import javax.swing.SwingConstants;
import javax.swing.JCheckBox;
import javax.swing.border.EmptyBorder;

public class FindPanel extends JPanel {
	private static final long serialVersionUID = 1L;	
	//
	private ExpressionTreeView expressionTreeView;
	//
	private JTextField textFieldFindWhat;
	private JLabel lblNotification;
	private JCheckBox chckbxRegularExpressions;
	//
	
	public FindPanel() {
		setBorder(new EmptyBorder(2, 2, 2, 2));
		initialize();
	}

	
	@Override
	public void setVisible(boolean visible) {
		lblNotification.setText("");
		super.setVisible(visible);
	}
	
	//
	// PACKAGE
	//
	void setExpressionTreeView(ExpressionTreeView expressionTreeView) {
		this.expressionTreeView = expressionTreeView;
	}
		
	//
	// PRIVATE
	//
	private void initialize() {
		setLayout(new BorderLayout(0, 0));
		
		JPanel mainPanel = new JPanel();
		add(mainPanel, BorderLayout.NORTH);
		mainPanel.setLayout(new BorderLayout(0, 0));
		
		JLabel lblFindWhat = new JLabel("Find What: ");
		mainPanel.add(lblFindWhat, BorderLayout.WEST);
		
		textFieldFindWhat = new JTextField();
		textFieldFindWhat.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if (expressionTreeView != null) {
					notifyFindOutcome(expressionTreeView.findNext(textFieldFindWhat.getText(), chckbxRegularExpressions.isSelected()));
				}
			}
		});
		mainPanel.add(textFieldFindWhat, BorderLayout.CENTER);
		textFieldFindWhat.setColumns(10);
		
		JPanel actionPanel = new JPanel();
		mainPanel.add(actionPanel, BorderLayout.EAST);
		actionPanel.setLayout(new GridLayout(0, 3, 2, 0));
		
		JButton btnNext = new JButton("Next");
		btnNext.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent arg0) {
				if (expressionTreeView != null) {
					notifyFindOutcome(expressionTreeView.findNext(textFieldFindWhat.getText(), chckbxRegularExpressions.isSelected()));
				}
			}
		});
		actionPanel.add(btnNext);
		
		JButton btnPrevious = new JButton("Previous");
		btnPrevious.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if (expressionTreeView != null) {
					notifyFindOutcome(expressionTreeView.findPrevious(textFieldFindWhat.getText(), chckbxRegularExpressions.isSelected()));
				}
			}
		});
		actionPanel.add(btnPrevious);
		
		JButton btnCLose = new JButton("Close");
		btnCLose.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				FindPanel.this.setVisible(false);
				FindPanel.this.invalidate();
			}
		});
		actionPanel.add(btnCLose);
		
		JPanel notificationPanel = new JPanel();
		mainPanel.add(notificationPanel, BorderLayout.SOUTH);
		notificationPanel.setLayout(new BorderLayout(0, 0));
		
		lblNotification = new JLabel("");
		lblNotification.setHorizontalAlignment(SwingConstants.LEFT);
		notificationPanel.add(lblNotification, BorderLayout.CENTER);
		
		chckbxRegularExpressions = new JCheckBox("Is Regular Expression");
		notificationPanel.add(chckbxRegularExpressions, BorderLayout.WEST);
	}
	
	private void notifyFindOutcome(boolean found) {
		if (found) {
			lblNotification.setText("");
		}
		else {
			lblNotification.setText(" : Search String '"+textFieldFindWhat.getText()+"' not found!");
		}
	}
}
