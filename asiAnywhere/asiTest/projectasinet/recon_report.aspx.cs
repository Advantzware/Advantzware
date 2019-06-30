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

public partial class recon_report : System.Web.UI.Page
{

    private bool bSort = true;

    protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
       
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "recon_report.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            labelcompany.Text = PrmComp;
            Session["Customers_Company"] = PrmComp;
            if (aUsers == "external")
            {
               
            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }
        
        string sCulture = ConfigurationManager.AppSettings["LCID"];
        if (!String.IsNullOrEmpty(sCulture))
        {
            int nCulture = int.Parse(sCulture);
            System.Threading.Thread.CurrentThread.CurrentCulture = new System.Globalization.CultureInfo(nCulture, false);
        }

        if (!Page.IsPostBack)
        {
            OutPutFile.Visible = false;
            HyperLink1.Visible = false;
        
            if (Session["User"] != null)
            {                
                lblUser.Text = UserLogin.UserName;
            }
            

            SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
            try
            {
                conn.Open();

                string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'recon_report.aspx' ";
                SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                DataSet ds = new DataSet();
                da.Fill(ds);

                foreach (DataRow dr in ds.Tables[0].Rows)
                {
                    bnkcodTextBox.Text = dr["field1"].ToString();                    
                    bevendTextBox.Text       = dr["field2"].ToString();
                    endvendTextBox.Text       = dr["field3"].ToString();
                    bechkdtTextBox.Text = dr["field4"].ToString();
                    endchkdtTextBox.Text = dr["field5"].ToString();
                    bechkTextBox.Text = dr["field6"].ToString();
                    endchkTextBox.Text = dr["field7"].ToString();
                    bejurTextBox.Text = dr["field8"].ToString();
                    endjurTextBox.Text = dr["field9"].ToString();
                    
                    

                    if (dr["chk_field1"].ToString() == "True")
                        CheckBox1.Checked = true;
                    else
                        CheckBox1.Checked = false;
                    if (dr["chk_field2"].ToString() == "True")
                        CheckBox2.Checked = true;
                    else
                        CheckBox2.Checked = false;
                    if (dr["chk_field3"].ToString() == "True")
                        CheckBox3.Checked = true;
                    else
                        CheckBox3.Checked = false;
                    if (dr["chk_field4"].ToString() == "True")
                        CheckBox4.Checked = true;
                    else
                        CheckBox4.Checked = false;

                    if (dr["chk_field5"].ToString() == "True")
                        CheckBox5.Checked = true;
                    else
                        CheckBox5.Checked = false;

                    if (dr["rd_field1"].ToString() == "unrec")
                        RadioButtonList1.SelectedIndex = 0;
                    if (dr["rd_field1"].ToString() == "rec")
                        RadioButtonList1.SelectedIndex = 1;
                    if (dr["rd_field1"].ToString() == "all")
                        RadioButtonList1.SelectedIndex = 2; 

                                 
                }

                conn.Close();
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
                

                if (endvendTextBox.Text == "")
                    endvendTextBox.Text = "zzzzzzzz";                

                if (endchkTextBox.Text == "")
                    endchkTextBox.Text = "99999999";
                if (endjurTextBox.Text == "")
                    endjurTextBox.Text = "99999999";               


                //RadioButtonList1.SelectedIndex = 0; 
            }
            catch { }
            
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
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        if (CheckBox1.Checked)
            HiddenField1.Value = "yes";
        else
            HiddenField1.Value = "no";
        if (CheckBox2.Checked)
            HiddenField2.Value = "yes";
        else
            HiddenField2.Value = "no";
        if (CheckBox3.Checked)
            HiddenField3.Value = "yes";
        else
            HiddenField3.Value = "no";
        if (CheckBox4.Checked)
            HiddenField4.Value = "yes";
        else
            HiddenField4.Value = "no";
        if (CheckBox5.Checked)
            HiddenField5.Value = "yes";
        else
            HiddenField5.Value = "no";

        if (RadioButtonList1.SelectedIndex == 0)
            HiddenField6.Value = "unrec";
        if (RadioButtonList1.SelectedIndex == 1)
            HiddenField6.Value = "rec";
        if (RadioButtonList1.SelectedIndex == 2)
            HiddenField6.Value = "all";

        
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmrecrpt"].DefaultValue = "recrpt";
        ObjectDataSource1.SelectParameters["prmbnkcod"].DefaultValue = bnkcodTextBox.Text.Trim();        
        ObjectDataSource1.SelectParameters["prmbegvend"].DefaultValue = bevendTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmendvend"].DefaultValue = endvendTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmbegchkdt"].DefaultValue = bechkdtTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmendchkdt"].DefaultValue = endchkdtTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmbegchk"].DefaultValue = bechkTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmendchk"].DefaultValue = endchkTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmbegjno"].DefaultValue = bejurTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmendjno"].DefaultValue = endjurTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmdep"].DefaultValue = HiddenField1.Value;
        ObjectDataSource1.SelectParameters["prmjournl"].DefaultValue = HiddenField2.Value;
        ObjectDataSource1.SelectParameters["prmclrunrec"].DefaultValue = HiddenField3.Value;
        ObjectDataSource1.SelectParameters["prmunclrunrec"].DefaultValue = HiddenField4.Value;
        ObjectDataSource1.SelectParameters["prmsrtvend"].DefaultValue = HiddenField5.Value;
        ObjectDataSource1.SelectParameters["prmprint"].DefaultValue = HiddenField6.Value;        
        //ObjectDataSource1.SelectParameters["prmOut"].DefaultValue = Convert.ToString(HiddenFieldPost.Value); 
        

        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();

            string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'recon_report.aspx' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            if (ds.Tables[0].Rows.Count == 0)
            {
                SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name , field1, field2, field3, field4, field5, field6, field7, field8, field9, chk_field1, chk_field2, chk_field3, chk_field4, chk_field5, rd_field1) values ('" + UserLogin.UserName + "','recon_report.aspx' , '" + bnkcodTextBox.Text.Trim() + "', '" + bevendTextBox.Text.Trim() + "', '" + endvendTextBox.Text.Trim() + "', '" + bechkdtTextBox.Text.Trim() + "', '" + endchkdtTextBox.Text.Trim() + "', '" + bechkTextBox.Text.Trim() + "', '" + endchkTextBox.Text.Trim() + "', '" + bejurTextBox.Text.Trim() + "', '" + endjurTextBox.Text.Trim() + "','" + CheckBox1.Checked + "','" + CheckBox2.Checked + "','" + CheckBox3.Checked + "','" + CheckBox4.Checked + "','" + CheckBox5.Checked + "','" + RadioButtonList1.SelectedValue + "')", conn);
                cmd_insert.ExecuteNonQuery();
            }
            else
            {
                SqlCommand cmd_update = new SqlCommand("update report_maintance set field1 = '" + bnkcodTextBox.Text.Trim() + "', field2 = '" + bevendTextBox.Text.Trim() + "', field3 = '" + endvendTextBox.Text.Trim() + "', field4 = '" + bechkdtTextBox.Text.Trim() + "', field5 = '" + endchkdtTextBox.Text.Trim() + "', field6 = '" + bechkTextBox.Text.Trim() + "', field7 = '" + endchkTextBox.Text.Trim() + "', field8 = '" + bejurTextBox.Text.Trim() + "', field9 = '" + endjurTextBox.Text.Trim() + "', chk_field1 = '" + CheckBox1.Checked + "', chk_field2 = '" + CheckBox2.Checked + "', chk_field3 = '" + CheckBox3.Checked + "', chk_field4 = '" + CheckBox4.Checked + "', chk_field5 = '" + CheckBox5.Checked + "', rd_field1 = '" + RadioButtonList1.SelectedValue + "' where user_name = '" + UserLogin.UserName + "' and prog_name =  'recon_report.aspx' ", conn);
                cmd_update.ExecuteNonQuery();
            }
            conn.Close();
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
            OutPutFile.Visible = true;
            HyperLink1.Visible = true;
            Label vpath = (Label)FormView1.FindControl("recrptLabel");
            HyperLink1.Text = vpath.Text;
            HyperLink1.NavigateUrl = @"/pdfs/" + vpath.Text;


            if (vpath.Text == "")
            {
                OutPutFile.Text = "No CSV Exists";
                
            }
        }
        catch { }       
        
    }

   }
