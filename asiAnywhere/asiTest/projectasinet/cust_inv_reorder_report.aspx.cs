
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

public partial class cust_inv_reorder_report : System.Web.UI.Page
{

     protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        //ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        //Response.Write(UserLogin.UserName);
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "cust_inv_reorder_report.aspx";
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

                        string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'cust_inv_reord_report.aspx'  ";
                        SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                        DataSet ds = new DataSet();
                        da.Fill(ds);
                        foreach (DataRow dr in ds.Tables[0].Rows)
                        {                           
                            BelocTextBox.Text = dr["field3"].ToString();
                            EndlocTextBox.Text = dr["field4"].ToString();
                            BeitemTextBox.Text = dr["field5"].ToString();
                            EnditemTextBox.Text = dr["field6"].ToString();
                            BecatTextBox.Text = dr["field7"].ToString();
                            EndCatTextBox.Text = dr["field8"].ToString();
                            DateTextBox.Text = dr["field9"].ToString();

                            if (dr["rd_field1"].ToString() == "T")
                                RadioButtonList1.SelectedIndex = 0;
                            if (dr["rd_field1"].ToString() == "R")
                                RadioButtonList1.SelectedIndex = 1;
                            if (dr["rd_field1"].ToString() == "A")
                                RadioButtonList1.SelectedIndex = 2;

                            if (dr["rd_field2"].ToString() == "P")
                                RadioButtonList1.SelectedIndex = 0;
                            if (dr["rd_field2"].ToString() == "M")
                                RadioButtonList1.SelectedIndex = 1;
                            if (dr["rd_field2"].ToString() == "A")
                                RadioButtonList1.SelectedIndex = 2;

                            if (dr["rd_field3"].ToString() == "L")
                                RadioButtonList1.SelectedIndex = 0;
                            if (dr["rd_field3"].ToString() == "R")
                                RadioButtonList1.SelectedIndex = 1;
                            if (dr["rd_field3"].ToString() == "A")
                                RadioButtonList1.SelectedIndex = 2;

                            if (dr["chk_field1"].ToString() == "Yes")
                                CheckBox1.Checked = true;
                            else
                                CheckBox1.Checked = false;

                            if (dr["chk_field2"].ToString() == "Yes")
                                CheckBox2.Checked = true;
                            else
                                CheckBox2.Checked = false;

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
                    TextBox2.ReadOnly = true;
                    Image3.Visible = false;

                    if (EnditemTextBox.Text == "")
                        EnditemTextBox.Text = "zzzzzzzzzzzzzzz";
                    if (EndlocTextBox.Text == "")
                        EndlocTextBox.Text = "zzzzzzzz";
                    if (EndCatTextBox.Text == "")
                        EndCatTextBox.Text = "zzzzz";
                    if (DateTextBox.Text == "")
                        DateTextBox.Text = "05/11/2005";

                }
            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }

            if (aUsers == "internal")
            {
                if (!Page.IsPostBack)
                {
                    SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
                    try
                    {
                        conn.Open();

                        string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'cust_inv_reord_report.aspx'  ";
                        SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                        DataSet ds = new DataSet();
                        da.Fill(ds);
                        foreach (DataRow dr in ds.Tables[0].Rows)
                        {
                            TextBox1.Text = dr["field1"].ToString();
                            TextBox2.Text = dr["field2"].ToString();
                            BelocTextBox.Text = dr["field3"].ToString();
                            EndlocTextBox.Text = dr["field4"].ToString();
                            BeitemTextBox.Text = dr["field5"].ToString();
                            EnditemTextBox.Text = dr["field6"].ToString();
                            BecatTextBox.Text = dr["field7"].ToString();
                            EndCatTextBox.Text = dr["field8"].ToString();
                            DateTextBox.Text = dr["field9"].ToString();

                            if (dr["rd_field1"].ToString() == "T")
                                RadioButtonList1.SelectedIndex = 0;
                            if (dr["rd_field1"].ToString() == "R")
                                RadioButtonList1.SelectedIndex = 1;
                            if (dr["rd_field1"].ToString() == "A")
                                RadioButtonList1.SelectedIndex = 2;

                            if (dr["rd_field2"].ToString() == "P")
                                RadioButtonList2.SelectedIndex = 0;
                            if (dr["rd_field2"].ToString() == "M")
                                RadioButtonList2.SelectedIndex = 1;
                            if (dr["rd_field2"].ToString() == "A")
                                RadioButtonList2.SelectedIndex = 2;

                            if (dr["rd_field3"].ToString() == "L")
                                RadioButtonList3.SelectedIndex = 0;
                            if (dr["rd_field3"].ToString() == "R")
                                RadioButtonList3.SelectedIndex = 1;
                            if (dr["rd_field3"].ToString() == "A")
                                RadioButtonList3.SelectedIndex = 2;

                            if (dr["chk_field1"].ToString() == "Yes")
                                CheckBox1.Checked = true;
                            else
                                CheckBox1.Checked = false;

                            if (dr["chk_field2"].ToString() == "Yes")
                                CheckBox2.Checked = true;
                            else
                                CheckBox2.Checked = false;

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

                    
                    if (EnditemTextBox.Text == "")
                        EnditemTextBox.Text = "zzzzzzzzzzzzzzz";
                    if (EndlocTextBox.Text == "")
                        EndlocTextBox.Text = "zzzzzzzz";
                    if (EndCatTextBox.Text == "")
                        EndCatTextBox.Text = "zzzzz";
                    if (DateTextBox.Text == "")
                        DateTextBox.Text = "05/11/2005";
                }
            }
        }
             
                       
            if (Session["User"] != null)
            {

                lblUser.Text = UserLogin.UserName;

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
        if (CheckBox1.Checked)
        {
            HiddenField1.Value = "Yes";
        }
        else
        {
            HiddenField1.Value = "No";
        }
        if (CheckBox2.Checked)
        {
            HiddenField2.Value = "Yes";
        }
        else
        {
            HiddenField2.Value = "No";
        }

        if (RadioButtonList1.SelectedIndex == 0)
            HiddenField3.Value = "T";
        if (RadioButtonList1.SelectedIndex == 1)
            HiddenField3.Value = "R";
        if (RadioButtonList1.SelectedIndex == 2)
            HiddenField3.Value = "A";

        if (RadioButtonList2.SelectedIndex == 0)
            HiddenField4.Value = "P";
        if (RadioButtonList2.SelectedIndex == 1)
            HiddenField4.Value = "M";
        if (RadioButtonList2.SelectedIndex == 2)
            HiddenField4.Value = "A";

        if (RadioButtonList3.SelectedIndex == 0)
            HiddenField5.Value = "L";
        if (RadioButtonList3.SelectedIndex == 1)
            HiddenField5.Value = "R";
        if (RadioButtonList3.SelectedIndex == 2)
            HiddenField5.Value = "A";




        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;

        ObjectDataSource1.SelectParameters["prmReAct"].DefaultValue = "PrintReprot";
        ObjectDataSource1.SelectParameters["prmBegCust"].DefaultValue = TextBox1.Text.Trim();

        ObjectDataSource1.SelectParameters["prmEndCust"].DefaultValue = TextBox2.Text.Trim(); ;
        ObjectDataSource1.SelectParameters["prmBegShipto"].DefaultValue = BelocTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndShitto"].DefaultValue = EndlocTextBox .Text.Trim();
        ObjectDataSource1.SelectParameters["prmBegItem"].DefaultValue = BeitemTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndItem"].DefaultValue = EnditemTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBegCat"].DefaultValue = BecatTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndCat"].DefaultValue = EndCatTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCustInv"].DefaultValue = HiddenField1.Value;
        ObjectDataSource1.SelectParameters["prmQOnHand"].DefaultValue = HiddenField2.Value;
        ObjectDataSource1.SelectParameters["prmTotalAlloc"].DefaultValue = HiddenField3.Value;
        ObjectDataSource1.SelectParameters["prmAsOf"].DefaultValue = DateTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmPrint"].DefaultValue = HiddenField4.Value;
        ObjectDataSource1.SelectParameters["prmPrintLotCtl"].DefaultValue = HiddenField5.Value;



        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();

            string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'cust_inv_reord_report.aspx' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            if (ds.Tables[0].Rows.Count == 0)
            {
                SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name, field1, field2, field3, field4, field5, field6, field7, field8, field9,rd_field1,rd_field2,rd_field3, chk_field1, chk_field2) values ('" + UserLogin.UserName + "','cust_inv_reord_report.aspx','" + TextBox1.Text.Trim() + "','" + TextBox2.Text.Trim() + "','" + BelocTextBox.Text.Trim() + "','" + EndlocTextBox.Text.Trim() + "','" + BeitemTextBox.Text.Trim() + "','" + EnditemTextBox.Text.Trim() + "','" + BecatTextBox.Text.Trim() + "','" + EndCatTextBox.Text.Trim() + "','" + DateTextBox.Text.Trim() + "','" + HiddenField3.Value + "','" + HiddenField4.Value + "','" + HiddenField5.Value + "','" + HiddenField1.Value + "','" + HiddenField2.Value + "')", conn);
                cmd_insert.ExecuteNonQuery();
            }
            else
            {
                SqlCommand cmd_update = new SqlCommand("update report_maintance set field1 = '" + TextBox1.Text.Trim() + "', field2 = '" + TextBox2.Text.Trim() + "', field3 = '" + BelocTextBox.Text.Trim() + "', field4 = '" + EndlocTextBox.Text.Trim() + "', field5 = '" + BeitemTextBox.Text.Trim() + "', field6 = '" + EnditemTextBox.Text.Trim() + "', field7 = '" + BecatTextBox.Text.Trim() + "', field8 = '" + EndCatTextBox.Text.Trim() + "', field9 = '" + DateTextBox.Text.Trim() + "', rd_field1 = '" + HiddenField3.Value + "', rd_field2 = '" + HiddenField4.Value + "', rd_field3 = '" + HiddenField5.Value + "', chk_field1 = '" + HiddenField1.Value + "', chk_field2 = '" + HiddenField2.Value + "' where user_name = '" + UserLogin.UserName + "' and prog_name ='cust_inv_reord_report.aspx' ", conn);
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
            Label vpath = (Label)FormView1.FindControl("vCustRepLabel");

            if (vpath.Text != "")
            {
                string path = vpath.Text;
                string path2 = @"/pdfs/" + path;
                Session["open_cust_reorder_list"] = path2;
                if (path2 != "")
                {
                    if (!Request.Browser.Browser.Contains("Safari"))
                        Response.Write("<script>window.open('print_cust_inv_reorder_report.aspx'); target='_blank'</script>");
                    else
                        Response.Redirect("cust_inv_reorder_report.aspx");
                }

            }
            else
            {
                Label1.Text = "No File Exists";
            }
        }
        catch
        {
            Label1.Text = "No File Exists";
        }
        finally
        {
            if (Label1.Text == "")
            {
                Response.Write("<script>window.location.href='cust_inv_reorder_report.aspx'</script>");
                
            }
        }
    }
    
   
}

