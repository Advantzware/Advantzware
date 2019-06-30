#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
using System.Data.SqlClient;
using System.Web;
using System.Text;
using System.IO;
using System.Web.UI;
#endregion

public partial class loadtaglook : System.Web.UI.Page
{

    private bool bSort = true;

    int seqence = 0;

    protected void Page_UnLoad(object sender, EventArgs e)
    {
                
    }

    protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
       
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "loadtag_report.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            
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
            Session["loadtaglook_tagno"] = Request.QueryString["tagno"];
            //reports load = new reports();
            //DataSet dsload = new DataSet();
            //dsload = load.SharpShooterLoadTag(UserLogin.UserName,"", Request.QueryString["tag1"],Request.QueryString["tag1"],Request.QueryString["tag1"],Request.QueryString["tag1"],Request.QueryString["tag1"],Request.QueryString["tag1"],Request.QueryString["tag1"],Request.QueryString["tag1"],Request.QueryString["tag1"],Request.QueryString["tag1"],Request.QueryString["tag1"],Request.QueryString["tag1"],Request.QueryString["tag1"],Request.QueryString["tag1"],Request.QueryString["tag1"],Request.QueryString["tag1"] Convert.ToString(Session["prmAction_corr"]), "Corr", "", Convert.ToString(Session["prmEstimate_corr"]), Convert.ToString(Session["prmCustomer_corr"]), Convert.ToString(Session["prmCustpart_corr"]), Convert.ToString(Session["prmFgItem_corr"]), Convert.ToString(Session["prmStyle_corr"]), Convert.ToString(Session["prmShipto_corr"]), Convert.ToString(Session["prmDie_corr"]), Convert.ToString(Session["prmCad_corr"]), Convert.ToString(Session["prmPlate_corr"]), Convert.ToString(Session["prmSingle_corr"]), Convert.ToString(Session["prmSet_corr"]), Convert.ToString(Session["prmTandem_corr"]), Convert.ToDecimal(Session["prmWidthFrom_corr"]), Convert.ToDecimal(Session["prmWidthTo_corr"]), Convert.ToDecimal(Session["prmLenFrom_corr"]), Convert.ToDecimal(Session["prmLenTo_corr"]), Convert.ToDecimal(Session["prmDepFrom_corr"]), Convert.ToDecimal(Session["prmDepTo_corr"]), Convert.ToString(Session["prmPartDscr_corr"]));
            try
            {
                GridView1.SelectedIndex = 0;
                seqence = Convert.ToInt32(((Label)GridView1.SelectedRow.FindControl("sequenc_no")).Text);
            }
            catch { }                                  
        }
        if (Convert.ToString(GridView1.SelectedIndex) != "")
        {
            try
            {
                seqence = Convert.ToInt32(((Label)GridView1.SelectedRow.FindControl("sequenc_no")).Text);
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

       
        

        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();

            string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'vendor_posting_report.aspx' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            //if (ds.Tables[0].Rows.Count == 0)
            //{
            //    SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name , field1, field2, field3, field4, field5, field6,field7, field8, chk_field1) values ('" + UserLogin.UserName + "','vendor_posting_report.aspx' , '" + postdateTextBox.Text.Trim() + "', '" + perTextBox.Text.Trim() + "', '" + bevendTextBox.Text.Trim() + "', '" + endvendTextBox.Text.Trim() + "', '" + beinvTextBox.Text.Trim() + "', '" + endinvTextBox.Text.Trim() + "', '" + beuserTextBox.Text.Trim() + "', '" + enduserTextBox.Text.Trim() + "', '" + HiddenField1.Value + "')", conn);
            //    cmd_insert.ExecuteNonQuery();
            //}
            //else
            //{
            //    SqlCommand cmd_update = new SqlCommand("update report_maintance set field1 = '" + postdateTextBox.Text.Trim() + "', field2 = '" + perTextBox.Text.Trim() + "', field3 = '" + bevendTextBox.Text.Trim() + "', field4 = '" + endvendTextBox.Text.Trim() + "', field5 = '" + beinvTextBox.Text.Trim() + "', field6 = '" + endinvTextBox.Text.Trim() + "',field7 = '" + beuserTextBox.Text.Trim() + "',field8 = '" + enduserTextBox.Text.Trim() + "', chk_field1 = '" + HiddenField1.Value + "' where user_name = '" + UserLogin.UserName + "' and prog_name =  'vendor_posting_report.aspx' ", conn);
            //    cmd_update.ExecuteNonQuery();
            //}
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


            
        
    }

    protected void Create_tag_Click(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());

        conn.Open();
        SqlDataAdapter da = new SqlDataAdapter("select temp1,temp2,temp3,temp4,temp5,temp6,temp7,temp8,temp9,temp10,temp11,temp12,temp13,temp14,temp15,temp16,temp17,temp18 from temp_table where file_name = '"+ UserLogin.UserName +"' and temp20 = '"+ Convert.ToString(Session["loadtaglook_tagno"]) +"' ", conn);
        DataSet ds = new DataSet();
        da.Fill(ds);


        StringBuilder str = new StringBuilder();
        
        int n = ds.Tables[0].Columns.Count - 1;

        for (int i = 0; i <= ds.Tables[0].Rows.Count - 1; i++)
        {
            for (int j = 0; j <= ds.Tables[0].Columns.Count - 1; j++)
            {
                str.Append('"');
                str.Append(ds.Tables[0].Rows[i][j].ToString());
                str.Append('"');
                for (int k = j; k < j + 1 && j < n; k++)
                {
                    str.Append(",");
                }
            }
            str.AppendFormat(Environment.NewLine);

        }
        
        string filePath = Server.MapPath("MyCSVFolder") + "\\" + UserLogin.UserName + "loadtag.txt";
        File.WriteAllText(filePath, str.ToString());

        SqlCommand cmd_delete = new SqlCommand("delete from temp_table where temp20 = '"+ Convert.ToString(Session["loadtaglook_tagno"]) +"'", conn);
        cmd_delete.ExecuteNonQuery();

        conn.Close();
        if (!ClientScript.IsStartupScriptRegistered("alert"))
        {
            Page.ClientScript.RegisterStartupScript

                (this.GetType(), "alert", "CallParentWindowAlert();", true);
        }
        Page.ClientScript.RegisterStartupScript(this.GetType(), "close", "<script language=javascript>self.close();</script>");

    }
    protected void insertbutton_click(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        Label temp1 = (Label)FormView1.FindControl("temp1TextBox");
        Label temp3 = (Label)FormView1.FindControl("temp3TextBox");
        Label temp4 = (Label)FormView1.FindControl("temp4TextBox");
        Label temp2 = (Label)FormView1.FindControl("temp2TextBox");
        Label temp5 = (Label)FormView1.FindControl("temp5TextBox");
        Label temp6 = (Label)FormView1.FindControl("temp6TextBox");
        TextBox temp7 = (TextBox)FormView1.FindControl("temp7TextBox");
        TextBox temp8 = (TextBox)FormView1.FindControl("temp8TextBox");
        TextBox temp9 = (TextBox)FormView1.FindControl("temp9TextBox");
        TextBox temp10 = (TextBox)FormView1.FindControl("temp11TextBox");
        TextBox temp11 = (TextBox)FormView1.FindControl("temp12TextBox");
        TextBox temp12 = (TextBox)FormView1.FindControl("temp10TextBox");
        Label temp13 = (Label)FormView1.FindControl("temp13TextBox");
        Label temp14 = (Label)FormView1.FindControl("temp14TextBox");
        TextBox temp15 = (TextBox)FormView1.FindControl("temp15TextBox");
        Label temp16 = (Label)FormView1.FindControl("temp16TextBox");
        Label temp17 = (Label)FormView1.FindControl("temp17TextBox");

        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());

        conn.Open();


        try
        {

            SqlCommand cmd_insert = new SqlCommand("insert into temp_table(file_name, temp1 , temp2, temp3, temp4, temp5, temp6, temp7,temp8, temp9, temp10,temp11,temp12,temp13,temp14,temp15,temp16,temp17,temp18,temp19,temp20) values ('" + UserLogin.UserName + "','" + temp1.Text.Trim() + "' , '" + temp2.Text.Trim() + "', '" + temp3.Text.Trim() + "', '" + temp4.Text.Trim() + "', '" + temp5.Text.Trim() + "', '" + temp6.Text.Trim() + "', '" + temp7.Text.Trim() + "', '" + temp8.Text.Trim() + "', '" + temp9.Text.Trim() + "','" + temp10.Text.Trim() + "','" + temp11.Text.Trim() + "','" + temp12.Text.Trim() + "','" + temp13.Text.Trim() + "','" + temp14.Text.Trim() + "','" + temp15.Text.Trim() + "','" + temp16.Text.Trim() + "','" + temp17.Text.Trim() + "','" + "addnewrec" + "','" + "" + "','" + Convert.ToString(Session["loadtaglook_tagno"]) + "')", conn);
            cmd_insert.ExecuteNonQuery();
        }
        catch (Exception ex)
        {
            Label1.Text = "Error :" + ex.Message + "<p>";
            conn.Close();
            return;
        }
        finally
        {
            conn.Close();
        }

        FormView1.ChangeMode(FormViewMode.ReadOnly);
        Response.Write("<script>window.location.href='loadtaglook.aspx?tagno=" + Convert.ToString(Session["loadtaglook_tagno"]) + "'</script>");
       


    }

    protected void updatebutton_click(object sender, EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        Label temp1 = (Label)FormView1.FindControl("temp1TextBox");
        Label temp3 = (Label)FormView1.FindControl("temp3TextBox");
        Label temp4 = (Label)FormView1.FindControl("temp4TextBox");
        Label temp2 = (Label)FormView1.FindControl("temp2TextBox");
        Label temp5 = (Label)FormView1.FindControl("temp5TextBox");
        Label temp6 = (Label)FormView1.FindControl("temp6TextBox");
        TextBox temp7 = (TextBox)FormView1.FindControl("temp7TextBox");
        TextBox temp8 = (TextBox)FormView1.FindControl("temp8TextBox");
        TextBox temp9 = (TextBox)FormView1.FindControl("temp9TextBox");
        TextBox temp10 = (TextBox)FormView1.FindControl("temp11TextBox");
        TextBox temp11 = (TextBox)FormView1.FindControl("temp12TextBox");
        TextBox temp12 = (TextBox)FormView1.FindControl("temp10TextBox");
        Label temp13 = (Label)FormView1.FindControl("temp13TextBox");
        Label temp14 = (Label)FormView1.FindControl("temp14TextBox");
        TextBox temp15 = (TextBox)FormView1.FindControl("temp15TextBox");
        Label temp16 = (Label)FormView1.FindControl("temp16TextBox");
        Label temp17 = (Label)FormView1.FindControl("temp17TextBox");



        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();            

            SqlCommand cmd_update = new SqlCommand("update temp_table set temp7 = '" + temp7.Text.Trim() + "', temp8 = '" + temp8.Text.Trim() + "', temp9 = '" + temp9.Text.Trim() + "', temp10 = '" + temp10.Text.Trim() + "', temp11 = '" + temp11.Text.Trim() + "', temp12 = '" + temp12.Text.Trim() + "', temp15 = '" + temp15.Text.Trim() + "' where seq_no = '" + seqence + "' ", conn);
                cmd_update.ExecuteNonQuery();
            
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

        FormView1.ChangeMode(FormViewMode.ReadOnly);
        Response.Write("<script>window.location.href='loadtaglook.aspx?tagno="+ Convert.ToString(Session["loadtaglook_tagno"]) +"'</script>");

    }

    protected void copyrecord_update(object sender, EventArgs e)
    {

        if (FormView1.CurrentMode == FormViewMode.Insert)
        {
            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];

            Label temp1 = (Label)FormView1.FindControl("temp1TextBox");
            Label temp3 = (Label)FormView1.FindControl("temp3TextBox");
            Label temp4 = (Label)FormView1.FindControl("temp4TextBox");
            Label temp2 = (Label)FormView1.FindControl("temp2TextBox");
            Label temp5 = (Label)FormView1.FindControl("temp5TextBox");
            Label temp6 = (Label)FormView1.FindControl("temp6TextBox");
            TextBox temp7 = (TextBox)FormView1.FindControl("temp7TextBox");
            TextBox temp8 = (TextBox)FormView1.FindControl("temp8TextBox");
            TextBox temp9 = (TextBox)FormView1.FindControl("temp9TextBox");
            TextBox temp10 = (TextBox)FormView1.FindControl("temp10TextBox");
            TextBox temp11 = (TextBox)FormView1.FindControl("temp11TextBox");
            TextBox temp12 = (TextBox)FormView1.FindControl("temp12TextBox");
            Label temp13 = (Label)FormView1.FindControl("temp13TextBox");
            Label temp14 = (Label)FormView1.FindControl("temp14TextBox");
            TextBox temp15 = (TextBox)FormView1.FindControl("temp15TextBox");
            Label temp16 = (Label)FormView1.FindControl("temp16TextBox");
            Label temp17 = (Label)FormView1.FindControl("temp17TextBox");

            SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        
            conn.Open();
           
            string cmd = "select * from temp_table where  seq_no = '" + seqence + "' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);
            
            //for (int j = 0; j < ds.Tables[0].Rows.Count; j++)
            //{
            //    SqlCommand cmd_insert = new SqlCommand("insert into temp_table(file_name, temp1 , temp2, temp3, temp4, temp5, temp6, temp7,temp8, temp9, temp10,temp11,temp12,temp13,temp14,temp15,temp16,temp17,temp18,temp19,temp20) values ('" + UserLogin.UserName + "','" + ds.Tables[0].Rows[j][0].ToString() + "' , '" + ds.Tables[0].Rows[j][1].ToString() + "', '" + ds.Tables[0].Rows[j][2].ToString() + "', '" + ds.Tables[0].Rows[j][3].ToString() + "', '" + ds.Tables[0].Rows[j][4].ToString() + "', '" + ds.Tables[0].Rows[j][5].ToString() + "', '" + ds.Tables[0].Rows[j][6].ToString() + "', '" + ds.Tables[0].Rows[j][7].ToString() + "', '" + ds.Tables[0].Rows[j][8].ToString() + "', '" + ds.Tables[0].Rows[j][9].ToString() + "','" + ds.Tables[0].Rows[j][10].ToString() + "','" + ds.Tables[0].Rows[j][11].ToString() + "','" + ds.Tables[0].Rows[j][12].ToString() + "','" + ds.Tables[0].Rows[j][13].ToString() + "','" + ds.Tables[0].Rows[j][14].ToString() + "','" + ds.Tables[0].Rows[j][15].ToString() + "','" + ds.Tables[0].Rows[j][16].ToString() + "','" + "addnewrec" + "','" + ds.Tables[0].Rows[j][18].ToString() + "','" + Convert.ToString(Session["loadtaglook_tagno"]) + "')", conn);
            //    cmd_insert.ExecuteNonQuery();
            //}

            temp1.Text = ds.Tables[0].Rows[0][0].ToString();
            temp2.Text = ds.Tables[0].Rows[0][1].ToString();
            temp3.Text = ds.Tables[0].Rows[0][2].ToString();
            temp4.Text = ds.Tables[0].Rows[0][3].ToString();
            temp5.Text = ds.Tables[0].Rows[0][4].ToString();
            temp6.Text = ds.Tables[0].Rows[0][5].ToString();
            temp7.Text = ds.Tables[0].Rows[0][6].ToString();
            temp8.Text = ds.Tables[0].Rows[0][7].ToString();
            temp9.Text = ds.Tables[0].Rows[0][8].ToString();
            temp10.Text = ds.Tables[0].Rows[0][11].ToString();
            temp11.Text = ds.Tables[0].Rows[0][9].ToString();
            temp12.Text = ds.Tables[0].Rows[0][10].ToString();
            temp13.Text = ds.Tables[0].Rows[0][12].ToString();
            temp14.Text = ds.Tables[0].Rows[0][13].ToString();
            temp15.Text = ds.Tables[0].Rows[0][14].ToString();
            temp16.Text = ds.Tables[0].Rows[0][15].ToString();
            temp17.Text = ds.Tables[0].Rows[0][16].ToString();
            
            conn.Close();

            

        }

    
    }

    protected void delete_Button_Click(object sender, EventArgs e)
    {
        Label seq_no = (Label)FormView1.FindControl("seq_noLabel");
        
         SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
         try
         {
             conn.Open();
             SqlCommand cmd_delete = new SqlCommand("delete from temp_table where seq_no = '" + seq_no.Text.Trim() + "' ", conn);
             cmd_delete.ExecuteNonQuery();
             conn.Close();
         }
         catch { }


         Response.Write("<script>window.location.href='loadtaglook.aspx?tagno=" + Convert.ToString(Session["loadtaglook_tagno"]) + "'</script>"); 
        
    }
    
    protected void gridview_selectedindexchange(object sender, EventArgs e)
    {
        seqence = Convert.ToInt32(((Label)GridView1.SelectedRow.FindControl("sequenc_no")).Text);
        

    }
    

   }
