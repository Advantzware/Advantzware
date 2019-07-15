
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
using System.Data.SqlClient;

#endregion

public partial class order_bal_by_po_report : System.Web.UI.Page
{

    protected void Page_Load(object sender, System.EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        //ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "order_bal_by_po_report.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();

            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            labelcompany.Text = PrmComp;
            Session["Customers_Company"] = labelcompany.Text;
            
            if (aUsers == "external")
            {
                if (!Page.IsPostBack)
                {
                    OutputLabel.Visible = false;
                    HyperLink1.Visible = false;
                    try
                    {
                        string UserId = UserLogin.UserName;
                        string aDefaultCust = null;
                        string aComp = null;

                        func1 user = new func1();
                        user.CheckUserCustomer(aComp, UserId, ref  aDefaultCust);
                        TextBox1.Text = aDefaultCust;
                        TextBox2.Text = aDefaultCust;
                    }
                    catch { }

                    SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
                    try
                    {
                        conn.Open();

                        string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'order_bal_by_po_report.aspx'  ";
                        SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                        DataSet ds = new DataSet();
                        da.Fill(ds);
                        foreach (DataRow dr in ds.Tables[0].Rows)
                        {
                            //TextBox1.Text = dr["field1"].ToString();
                            //TextBox2.Text = dr["field1"].ToString();
                            TextBox5.Text = dr["field3"].ToString();
                            TextBox6.Text = dr["field4"].ToString();
                            BeginCustpoTextBox.Text = dr["field5"].ToString();
                            EndCustpoTextBox.Text = dr["field6"].ToString();
                            BeginjobTextBox.Text = dr["field7"].ToString();
                            EndjobTextBox.Text = dr["field8"].ToString();
                            beginjob2TextBox.Text = dr["field9"].ToString();
                            endjob2TextBox.Text = dr["field10"].ToString();
                            TextBox3.Text = dr["field11"].ToString();
                            TextBox4.Text = dr["field12"].ToString();
                            BeginSalesmanTextBox.Text = dr["field13"].ToString();
                            EndingSalesmanTextBox.Text = dr["field14"].ToString();
                            olderTextbox.Text = dr["field15"].ToString();
                            asofTextBox.Text = dr["field16"].ToString();

                            if (dr["rd_field1"].ToString() == "PO#")
                                RadioButtonList1.SelectedIndex = 0;
                            if (dr["rd_field1"].ToString() == "Item")
                                RadioButtonList1.SelectedIndex = 1;
                            if (dr["rd_field1"].ToString() == "Cust Part")
                                RadioButtonList1.SelectedIndex = 2;
                            if (dr["rd_field1"].ToString() == "Fg Item Name")
                                RadioButtonList1.SelectedIndex = 3;
                            if (dr["rd_field1"].ToString() == "Order#")
                                RadioButtonList1.SelectedIndex = 4;
                            if (dr["rd_field1"].ToString() == "Due Date")
                                RadioButtonList1.SelectedIndex = 5;

                            if (dr["rd_field2"].ToString() == "Open")
                                RadioButtonList2.SelectedIndex = 0;
                            if (dr["rd_field2"].ToString() == "Closed")
                                RadioButtonList2.SelectedIndex = 1;
                            if (dr["rd_field2"].ToString() == "All")
                                RadioButtonList2.SelectedIndex = 2;

                            if (dr["rd_field3"].ToString() == "Open")
                                RadioButtonList3.SelectedIndex = 0;
                            if (dr["rd_field3"].ToString() == "Closed")
                                RadioButtonList3.SelectedIndex = 1;
                            if (dr["rd_field3"].ToString() == "All")
                                RadioButtonList3.SelectedIndex = 2;

                            if (dr["rd_field4"].ToString() == "Invoice Amount")
                                RadioButtonList4.SelectedIndex = 0;
                            if (dr["rd_field4"].ToString() == "Balance Due")
                                RadioButtonList4.SelectedIndex = 1;

                            if (dr["rd_field5"].ToString() == "Header")
                                RadioButtonList5.SelectedIndex = 0;
                            if (dr["rd_field5"].ToString() == "Line")
                                RadioButtonList5.SelectedIndex = 1;

                            if (dr["chk_field2"].ToString() == "Yes")
                                CheckBox4.Checked = true;
                            else
                                CheckBox4.Checked = false;

                            if (dr["chk_field3"].ToString() == "Yes")
                                CheckBox5.Checked = true;
                            else
                                CheckBox5.Checked = false;

                            if (dr["chk_field4"].ToString() == "Yes")
                                CheckBox6.Checked = true;
                            else
                                CheckBox6.Checked = false;

                            if (dr["chk_field1"].ToString() == "Yes")
                                PageBreakCheckbox.Checked = true;
                            else
                                PageBreakCheckbox.Checked = false;

                            if (dr["chk_field5"].ToString() == "Yes")
                                Checkbox8.Checked = true;
                            else
                                Checkbox8.Checked = false;
                            if (dr["chk_field6"].ToString() == "True")
                                CheckBox_SchRel.Checked = true;
                            else
                                CheckBox_SchRel.Checked = false;


                        }
                    }
                    catch
                    {
                        conn.Close();
                    }
                    finally
                    {
                        conn.Close();
                    }


                    
                    try
                    {                       
                        TextBox2.ReadOnly = true;
                        Image12.Visible = false;
                    }
                    catch { }


                    if (TextBox5.Text == "")
                        TextBox5.Text = "01/01/2008";
                    if (TextBox6.Text == "")
                        TextBox6.Text = "12/31/2009";
                    if (EndCustpoTextBox.Text == "")
                        EndCustpoTextBox.Text = "zzzzzzzzzzzzzzz";
                    if (TextBox4.Text == "")
                        TextBox4.Text = "zzzzzzzzzzzzzzz";
                    if (EndingSalesmanTextBox.Text == "")
                        EndingSalesmanTextBox.Text = "zzz";
                    if (olderTextbox.Text == "")
                        olderTextbox.Text = "0";
                    if (asofTextBox.Text == "")
                        asofTextBox.Text = "09/12/2008";
                    if (beginjob2TextBox.Text == "")
                        beginjob2TextBox.Text = "-00";
                    if (endjob2TextBox.Text == "")
                        endjob2TextBox.Text = "-09";
                    if (EndjobTextBox.Text == "")
                        EndjobTextBox.Text = "999999";
                    
                    if (Session["User"] != null)
                    {
                        lblUser.Text = UserLogin.UserName;
                    }
                }

            }
            if (aUsers == "internal")
            {                
                if (!Page.IsPostBack)
                {
                    OutputLabel.Visible = false;
                    HyperLink1.Visible = false;

                    
                      SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
                    try
                    {
                        conn.Open();

                        string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'order_bal_by_po_report.aspx'  ";
                        SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                        DataSet ds = new DataSet();
                        da.Fill(ds);
                        foreach (DataRow dr in ds.Tables[0].Rows)
                        {
                             
                             TextBox1.Text = dr["field1"].ToString();
                             TextBox2.Text = dr["field2"].ToString();
                             TextBox5.Text = dr["field3"].ToString();
                             TextBox6.Text = dr["field4"].ToString();
                             BeginCustpoTextBox.Text = dr["field5"].ToString();
                             EndCustpoTextBox.Text = dr["field6"].ToString();
                             BeginjobTextBox.Text = dr["field7"].ToString();
                             EndjobTextBox.Text = dr["field8"].ToString();
                             beginjob2TextBox.Text = dr["field9"].ToString();
                             endjob2TextBox.Text = dr["field10"].ToString();
                             TextBox3.Text = dr["field11"].ToString();
                             TextBox4.Text = dr["field12"].ToString();
                             BeginSalesmanTextBox.Text = dr["field13"].ToString();
                             EndingSalesmanTextBox.Text = dr["field14"].ToString();
                             olderTextbox.Text = dr["field15"].ToString();
                             asofTextBox.Text = dr["field16"].ToString();

                             if (dr["rd_field1"].ToString() == "PO#")
                                 RadioButtonList1.SelectedIndex = 0;
                             if (dr["rd_field1"].ToString() == "Item")
                                 RadioButtonList1.SelectedIndex = 1;
                             if (dr["rd_field1"].ToString() == "Cust Part")
                                 RadioButtonList1.SelectedIndex = 2;
                             if (dr["rd_field1"].ToString() == "Fg Item Name")
                                 RadioButtonList1.SelectedIndex = 3;
                             if (dr["rd_field1"].ToString() == "Order#")
                                 RadioButtonList1.SelectedIndex = 4;
                             if (dr["rd_field1"].ToString() == "Due Date")
                                 RadioButtonList1.SelectedIndex = 5;

                             if (dr["rd_field2"].ToString() == "Open")
                                 RadioButtonList2.SelectedIndex = 0;
                             if (dr["rd_field2"].ToString() == "Closed")
                                 RadioButtonList2.SelectedIndex = 1;
                             if (dr["rd_field2"].ToString() == "All")
                                 RadioButtonList2.SelectedIndex = 2;

                             if (dr["rd_field3"].ToString() == "Open")
                                 RadioButtonList3.SelectedIndex = 0;
                             if (dr["rd_field3"].ToString() == "Closed")
                                 RadioButtonList3.SelectedIndex = 1;
                             if (dr["rd_field3"].ToString() == "All")
                                 RadioButtonList3.SelectedIndex = 2;

                             if (dr["rd_field4"].ToString() == "Invoice Amount")
                                 RadioButtonList4.SelectedIndex = 0;
                             if (dr["rd_field4"].ToString() == "Balance Due")
                                 RadioButtonList4.SelectedIndex = 1;

                             if (dr["rd_field5"].ToString() == "Header")
                                 RadioButtonList5.SelectedIndex = 0;
                             if (dr["rd_field5"].ToString() == "Line")
                                 RadioButtonList5.SelectedIndex = 1;

                             if (dr["chk_field2"].ToString() == "Yes")
                                 CheckBox4.Checked = true;
                             else
                                 CheckBox4.Checked = false;

                             if (dr["chk_field3"].ToString() == "Yes")
                                 CheckBox5.Checked = true;
                             else
                                 CheckBox5.Checked = false;

                             if (dr["chk_field4"].ToString() == "Yes")
                                 CheckBox6.Checked = true;
                             else
                                 CheckBox6.Checked = false;

                             if (dr["chk_field1"].ToString() == "Yes")
                                 PageBreakCheckbox.Checked = true;
                             else
                                 PageBreakCheckbox.Checked = false;

                             if (dr["chk_field5"].ToString() == "Yes")
                                 Checkbox8.Checked = true;
                             else
                                 Checkbox8.Checked = false;
                             if (dr["chk_field6"].ToString() == "True")
                                 CheckBox_SchRel.Checked = true;
                             else
                                 CheckBox_SchRel.Checked = false;


                         }

                     }
                     catch
                     {
                         conn.Close();
                     }
                     finally
                     {
                         conn.Close();
                     }


                     if (TextBox5.Text == "")
                         TextBox5.Text = "01/01/2008";
                     if (TextBox6.Text == "")
                         TextBox6.Text = "12/31/2009";
                     if (EndCustpoTextBox.Text == "")
                         EndCustpoTextBox.Text = "zzzzzzzzzzzzzzz";
                     if (TextBox4.Text == "")
                         TextBox4.Text = "zzzzzzzzzzzzzzz";
                     if (EndingSalesmanTextBox.Text == "")
                         EndingSalesmanTextBox.Text = "zzz";
                     if (olderTextbox.Text == "")
                         olderTextbox.Text = "0";
                     if (asofTextBox.Text == "")
                         asofTextBox.Text = "09/12/2008";
                     if (beginjob2TextBox.Text == "")
                         beginjob2TextBox.Text = "-00";
                     if (endjob2TextBox.Text == "")
                         endjob2TextBox.Text = "-09";
                     if (EndjobTextBox.Text == "")
                         EndjobTextBox.Text = "999999";


                    if (Session["User"] != null)
                    {
                        lblUser.Text = UserLogin.UserName;
                    }
                }

            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }

        
    }

    protected void hlnkLogOut_Click(object sender, EventArgs e)
    {

        string sLoginURL = ConfigurationManager.AppSettings["LoginFile"];
        if (sLoginURL == "")
        {
            Response.Write("<script language=javascript>alert('" + "Login page isn’t set" + "!');</script>");
            return;
        }

        Page.Session.Clear();
        Response.Redirect(sLoginURL);
    }

    protected void Back_tomenu_Click(object sender, EventArgs e)
    {
        Response.Redirect("menu.aspx");
    }

    protected void submitbutton_click(object sender, EventArgs e)
    {
        if (RadioButtonList1.SelectedIndex == 0)
            HiddenField1.Value = "PO#";
        if (RadioButtonList1.SelectedIndex == 1)
            HiddenField1.Value = "Item";
        if (RadioButtonList1.SelectedIndex == 2)
            HiddenField1.Value = "Cust Part";
        if (RadioButtonList1.SelectedIndex == 3)
            HiddenField1.Value = "Fg Item Name";
        if (RadioButtonList1.SelectedIndex == 4)
            HiddenField1.Value = "Order#";
        if (RadioButtonList1.SelectedIndex == 5)
            HiddenField1.Value = "Due Date";
        if (RadioButtonList2.SelectedIndex == 0)
            HiddenField2.Value = "Open";
        if (RadioButtonList2.SelectedIndex == 1)
            HiddenField2.Value = "Closed";
        if (RadioButtonList2.SelectedIndex == 2)
            HiddenField2.Value = "All";
        if (RadioButtonList3.SelectedIndex == 0)
            HiddenField3.Value = "Open";
        if (RadioButtonList3.SelectedIndex == 1)
            HiddenField3.Value = "Closed";
        if (RadioButtonList3.SelectedIndex == 2)
            HiddenField3.Value = "All";
        if (RadioButtonList4.SelectedIndex == 0)
            HiddenField4.Value = "Invoice Amount";
        if (RadioButtonList4.SelectedIndex == 1)
            HiddenField4.Value = "Balance Due";
        if (RadioButtonList5.SelectedIndex == 0)
            HiddenField5.Value = "Header";
        if (RadioButtonList5.SelectedIndex == 1)
            HiddenField5.Value = "Line";

        
        if (CheckBox4.Checked)
        {
            HiddenField9.Value = "Yes";
        }
        if (CheckBox5.Checked)
        {
            HiddenField10.Value = "Yes";
        }
        if (CheckBox6.Checked)
        {
            HiddenField11.Value = "Yes";
        }
        if (PageBreakCheckbox.Checked)
        {
            HiddenField12.Value = "Yes";
        }
        if (Checkbox8.Checked)
        {
            HiddenField13.Value = "Yes";
        }
        

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        //ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "order_bal_by_po_report.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();

            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);


            if (aUsers == "external")
            {
                ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                ObjectDataSource1.SelectParameters["prmActBal"].DefaultValue = "OrderBal";
                ObjectDataSource1.SelectParameters["prmBegcust"].DefaultValue = TextBox1.Text.Trim();
                ObjectDataSource1.SelectParameters["prmEndCust"].DefaultValue = TextBox1.Text.Trim();
                ObjectDataSource1.SelectParameters["prmBegOrdDate"].DefaultValue = TextBox5.Text.Trim();
                ObjectDataSource1.SelectParameters["prmEndOrdDate"].DefaultValue = TextBox6.Text.Trim();
                ObjectDataSource1.SelectParameters["prmBegCustPo"].DefaultValue = BeginCustpoTextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmEndCustPo"].DefaultValue = EndCustpoTextBox.Text.Trim();

                ObjectDataSource1.SelectParameters["prmBegJob"].DefaultValue = BeginjobTextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmEndJob"].DefaultValue = EndjobTextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmBegJob2"].DefaultValue = beginjob2TextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmEndJob2"].DefaultValue = endjob2TextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmBegItem"].DefaultValue = TextBox3.Text.Trim();
                ObjectDataSource1.SelectParameters["prmEndItem"].DefaultValue = TextBox4.Text.Trim();
                ObjectDataSource1.SelectParameters["prmBegSman"].DefaultValue = BeginSalesmanTextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmEndSman"].DefaultValue = EndingSalesmanTextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmSort"].DefaultValue = HiddenField1.Value;
                ObjectDataSource1.SelectParameters["prmJobStat"].DefaultValue = HiddenField2.Value;
                ObjectDataSource1.SelectParameters["prmOrdStat"].DefaultValue = HiddenField3.Value;
                ObjectDataSource1.SelectParameters["prmPrint"].DefaultValue = HiddenField4.Value;
                ObjectDataSource1.SelectParameters["prmPofrom"].DefaultValue = HiddenField5.Value;

                ObjectDataSource1.SelectParameters["prmUnderrun"].DefaultValue = HiddenField9.Value;
                ObjectDataSource1.SelectParameters["prmJobqty"].DefaultValue = HiddenField10.Value;
                ObjectDataSource1.SelectParameters["prmZerobal"].DefaultValue = HiddenField10.Value;

                ObjectDataSource1.SelectParameters["prmDays"].DefaultValue = olderTextbox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmDate"].DefaultValue = asofTextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmZeroqoh"].DefaultValue = HiddenField13.Value;
                ObjectDataSource1.SelectParameters["prmPageBreak"].DefaultValue = HiddenField12.Value;
                ObjectDataSource1.SelectParameters["prmSchRel"].DefaultValue = Convert.ToString(CheckBox_SchRel.Checked);

            }
            if (aUsers == "internal")
            {
                ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                ObjectDataSource1.SelectParameters["prmActBal"].DefaultValue = "OrderBal";
                ObjectDataSource1.SelectParameters["prmBegcust"].DefaultValue = TextBox1.Text.Trim();
                ObjectDataSource1.SelectParameters["prmEndCust"].DefaultValue = TextBox2.Text.Trim();
                ObjectDataSource1.SelectParameters["prmBegOrdDate"].DefaultValue = TextBox5.Text.Trim();
                ObjectDataSource1.SelectParameters["prmEndOrdDate"].DefaultValue = TextBox6.Text.Trim();
                ObjectDataSource1.SelectParameters["prmBegCustPo"].DefaultValue = BeginCustpoTextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmEndCustPo"].DefaultValue = EndCustpoTextBox.Text.Trim();

                ObjectDataSource1.SelectParameters["prmBegJob"].DefaultValue = BeginjobTextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmEndJob"].DefaultValue = EndjobTextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmBegJob2"].DefaultValue = beginjob2TextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmEndJob2"].DefaultValue = endjob2TextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmBegItem"].DefaultValue = TextBox3.Text.Trim();
                ObjectDataSource1.SelectParameters["prmEndItem"].DefaultValue = TextBox4.Text.Trim();
                ObjectDataSource1.SelectParameters["prmBegSman"].DefaultValue = BeginSalesmanTextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmEndSman"].DefaultValue = EndingSalesmanTextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmSort"].DefaultValue = HiddenField1.Value;
                ObjectDataSource1.SelectParameters["prmJobStat"].DefaultValue = HiddenField2.Value;
                ObjectDataSource1.SelectParameters["prmOrdStat"].DefaultValue = HiddenField3.Value;
                ObjectDataSource1.SelectParameters["prmPrint"].DefaultValue = HiddenField4.Value;
                ObjectDataSource1.SelectParameters["prmPofrom"].DefaultValue = HiddenField5.Value;

                ObjectDataSource1.SelectParameters["prmUnderrun"].DefaultValue = HiddenField9.Value;
                ObjectDataSource1.SelectParameters["prmJobqty"].DefaultValue = HiddenField10.Value;
                ObjectDataSource1.SelectParameters["prmZerobal"].DefaultValue = HiddenField10.Value;

                ObjectDataSource1.SelectParameters["prmDays"].DefaultValue = olderTextbox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmDate"].DefaultValue = asofTextBox.Text.Trim();
                ObjectDataSource1.SelectParameters["prmZeroqoh"].DefaultValue = HiddenField13.Value;
                ObjectDataSource1.SelectParameters["prmPageBreak"].DefaultValue = HiddenField12.Value;
                ObjectDataSource1.SelectParameters["prmSchRel"].DefaultValue = Convert.ToString(CheckBox_SchRel.Checked);
            }

        }


        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();

            string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'order_bal_by_po_report.aspx' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);           

            if (ds.Tables[0].Rows.Count == 0)
            {
                SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name, field1, field2, field3, field4, field5, field6, field7, field8, field9, field10, field11, field12, field13, field14, rd_field1, rd_field2, rd_field3, rd_field4, chk_field1, chk_field2, chk_field3, chk_field4, rd_field5, field15, field16, chk_field5,chk_field6) values ('" + UserLogin.UserName + "','order_bal_by_po_report.aspx','" + TextBox1.Text.Trim() + "','" + TextBox2.Text.Trim() + "','" + TextBox5.Text.Trim() + "','" + TextBox6.Text.Trim() + "','" + BeginCustpoTextBox.Text.Trim() + "','" + EndCustpoTextBox.Text.Trim() + "','" + BeginjobTextBox.Text.Trim() + "','" + EndjobTextBox.Text.Trim() + "','" + beginjob2TextBox.Text.Trim() + "','" + endjob2TextBox.Text.Trim() + "','" + TextBox3.Text.Trim() + "','" + TextBox4.Text.Trim() + "','" + BeginSalesmanTextBox.Text.Trim() + "','" + EndingSalesmanTextBox.Text.Trim() + "','" + HiddenField1.Value + "','" + HiddenField2.Value + "','" + HiddenField3.Value + "','" + HiddenField4.Value + "','" + HiddenField12.Value + "','" + HiddenField9.Value + "','" + HiddenField10.Value + "','" + HiddenField11.Value + "','" + HiddenField5.Value + "','" + olderTextbox.Text.Trim() + "','" + asofTextBox.Text.Trim() + "','" + HiddenField13.Value + "','"+ Convert.ToString(CheckBox_SchRel.Checked) +"')", conn);
                cmd_insert.ExecuteNonQuery();
            }
            else
            {
                SqlCommand cmd_update = new SqlCommand("update report_maintance set field1 = '" + TextBox1.Text.Trim() + "', field2 = '" + TextBox2.Text.Trim() + "', field3 = '" + TextBox5.Text.Trim() + "', field4 = '" + TextBox6.Text.Trim() + "', field5 = '" + BeginCustpoTextBox.Text.Trim() + "', field6 = '" + EndCustpoTextBox.Text.Trim() + "', field7 = '" + BeginjobTextBox.Text.Trim() + "', field8 = '" + EndjobTextBox.Text.Trim() + "', field9 = '" + beginjob2TextBox.Text.Trim() + "', field10 = '" + endjob2TextBox.Text.Trim() + "', field11 = '" + TextBox3.Text.Trim() + "', field12 = '" + TextBox4.Text.Trim() + "', field13 =  '" + BeginSalesmanTextBox.Text.Trim() + "', field14 = '" + EndingSalesmanTextBox.Text.Trim() + "', rd_field1 = '" + HiddenField1.Value + "', rd_field2 = '" + HiddenField2.Value + "', rd_field3 = '" + HiddenField3.Value + "', rd_field4 = '" + HiddenField4.Value + "', chk_field1 = '" + HiddenField12.Value + "', chk_field2 = '" + HiddenField9.Value + "', chk_field3 = '" + HiddenField10.Value + "', chk_field4 = '" + HiddenField11.Value + "', rd_field5 = '" + HiddenField5.Value + "', field15 = '" + olderTextbox.Text.Trim() + "', field16 = '" + asofTextBox.Text.Trim() + "', chk_field5 = '" + HiddenField13.Value + "', chk_field6 = '" + Convert.ToString(CheckBox_SchRel.Checked) + "' where user_name = '" + UserLogin.UserName + "' and prog_name ='order_bal_by_po_report.aspx' ", conn);
                cmd_update.ExecuteNonQuery();
            }
            
        }
        catch (Exception ex)
        {
            Label1.Text = "Error :" + ex.Message + "<p>";
            conn.Close();
        }
        finally
        {
            conn.Close();
        }


        try
        {
            OutputLabel.Visible = true;
            HyperLink1.Visible = true;
            Label path = (Label)FormView1.FindControl("vFileLabel");
            HyperLink1.Text = path.Text;
            HyperLink1.NavigateUrl = @"/pdfs/" + path.Text;

            if (path.Text == "")
            {
                Label1.Text = "No Csv Exists";
                Response.Write("<script>window.location.href='order_bal_by_po_report.aspx'</script>");
            }
        }
        catch
        {

        }

    }
    
}
